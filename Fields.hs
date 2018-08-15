{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Fields
  ( FieldGenerators
  , Macros
  , generatorFields
  , generateFields
  , parseGenerators
  , languageGenerator
  , mapGenerator
  ) where

import           Control.Arrow (first)
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer.Lazy (WriterT(..), Writer, writer, runWriter, tell, mapWriter, censor)
import qualified Data.Aeson.Types as JSON
import           Data.Char (isAlphaNum)
import           Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import           Data.Maybe (fromMaybe, isJust, maybeToList)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.ICU as TU
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Vector as V

import           Util
import           ISO639
import           Document

-- |A 'Value' generator for a single metadata field
data Generator
  = GeneratorString !T.Text -- ^fixed text value
  | GeneratorField !T.Text -- ^copy input field
  | GeneratorMap
    { _generatorMap :: !(Value -> Generator) -- ^apply this function to the resulting value
    , _generator :: Generator
    }
  | GeneratorList [Generator] -- ^concatenate ('mconcat') all the values
  | GeneratorPaste [Generator] -- ^join ('T.concat') all the values, producing the cross-product
  | GeneratorWith
    { _generatorWith :: !(HMap.HashMap T.Text Generator) -- ^assign local variables to replace input
    , _generator :: Generator
    }

-- |Generate a static string
instance IsString Generator where
  fromString = GeneratorString . fromString

-- |Merge generators using 'GeneratorList'
instance Semigroup Generator where
  (<>) = mappend

-- |Merge generators using 'GeneratorList'
instance Monoid Generator where
  mempty = GeneratorList []
  mappend (GeneratorList []) g = g
  mappend g (GeneratorList []) = g
  mappend (GeneratorList a) (GeneratorList b) = GeneratorList (a ++ b)
  mappend (GeneratorList a) b = GeneratorList (a ++ [b])
  mappend a (GeneratorList b) = GeneratorList (a : b)
  mappend a b = GeneratorList [a, b]

valuesGenerator :: [T.Text] -> Generator
valuesGenerator = foldMap GeneratorString

-- |Encapusulate a generator value with the set of input fields
type FieldsT = WriterT (HSet.HashSet T.Text)
type Fields = FieldsT Identity
type Parser = FieldsT JSON.Parser

-- |A set of generator macros that can be applied as functions inside generators
type Macros = HMap.HashMap T.Text (Fields Generator)

-- |An entire metadata cross-walk, mapping a set of output fields to their 'Generator'
type FieldGenerators = Fields (HMap.HashMap T.Text Generator)

instance Semigroup FieldGenerators where
  a <> b = writer $ runWriter a <> runWriter b

instance Monoid FieldGenerators where
  mempty = writer (mempty, mempty)
  mappend a b = writer $ mappend (runWriter a) (runWriter b)

unTWriter :: Monad m => WriterT w m a -> m (Writer w a)
unTWriter = fmap writer . runWriterT

reWriter :: Monad m => Writer w a -> WriterT w m a
reWriter = writer . runWriter

-- |Extract the set of input fields
generatorFields :: Fields a -> HSet.HashSet T.Text
generatorFields = snd . runWriter

-- |Generate a single 'Value' given a set of input metadata values and a field 'Generator'
generate :: Metadata -> Generator -> Value
generate _ (GeneratorString x) = value x
generate m (GeneratorField f) = getMetadata m f
generate m (GeneratorMap f g) = generate m $ f $ generate m g
generate m (GeneratorList l) = foldMap (generate m) l
generate _ (GeneratorPaste []) = value T.empty
generate m (GeneratorPaste [x]) = generate m x
generate m (GeneratorPaste (g:l)) = Value
  [ x <> y
  | x <- values $ generate m g
  , y <- values $ generate m (GeneratorPaste l)
  ]
generate m (GeneratorWith gm g) = generate (HMap.map (generate m) gm) g

-- |Translate metadata using a cross-walk
generateFields :: FieldGenerators -> Metadata -> Metadata
generateFields g m = HMap.map (generate m) $ fst $ runWriter g

-- |Do we consider this character a normal part of a field name?  (This is just a heuristic -- any character is valid in a field name.)
isFieldChar :: Char -> Bool
isFieldChar '_' = True
isFieldChar '.' = True
isFieldChar c = isAlphaNum c

parseField :: Monad m => T.Text -> FieldsT m Generator
parseField f = GeneratorField f <$ tell (HSet.singleton f)

liftWith :: (forall c . a -> (b -> JSON.Parser c) -> d -> JSON.Parser c)
                     -> a -> (b ->      Parser e) -> d ->      Parser e
liftWith with a p = WriterT . (with a) (runWriterT . p)

parseInField :: T.Text -> Parser a -> Parser a
parseInField f = WriterT . inField f . runWriterT

-- |Parse a string with \$field substitutions.
parseSubst :: Monad m => T.Text -> FieldsT m Generator
parseSubst = fmap (simplify . merge) . subst where
  simplify [] = GeneratorString T.empty
  simplify [x] = x
  simplify l = GeneratorPaste l
  merge (GeneratorString a : GeneratorString b : r) =
    merge (GeneratorString (a <> b) : r)
  merge (x : r) = x : merge r
  merge [] = []
  subst "" = return []
  subst (T.breakOn "$" -> (p, d)) =
    (if T.null p then id else (GeneratorString p :))
    <$> case T.uncons d of
      Nothing -> return []
      Just (~'$', b) -> case T.uncons b of
        Just ('$', r) -> (GeneratorString (T.singleton '$') :) <$> subst r
        Just ('{', (T.break ('}' ==) -> (v, T.uncons -> Just (~'}', r)))) ->
          (:) <$> parseField v <*> subst r
        Just (c, T.span isFieldChar -> (v, r)) | isFieldChar c ->
          (:) <$> parseField (c `T.cons` v) <*> subst r
        _ -> fail "trailing/unterminated '$': expecting ${field}, $field, or $$"

-- |Parse a macro application, checking to make sure arguments match inputs
parseGeneratorMacro :: Macros -> Fields Generator -> JSON.Object -> Parser Generator
parseGeneratorMacro g (runWriter -> (m, ma)) o
  | not (HSet.null moa) = fail $ "missing arguments: " ++ show (HSet.toList moa)
  | not (HSet.null oma) = fail $ "extra arguments: " ++ show (HSet.toList oma)
  | otherwise = flip GeneratorWith m <$> HMap.traverseWithKey (\k -> parseInField k . parseGenerator g) o
  where
  oa = HSet.fromMap $ void $ o
  moa = ma `HSet.difference` oa
  oma = oa `HSet.difference` ma

-- |Parse a single field from a JSON object as a generator
parseGeneratorKey :: Macros -> T.Text -> JSON.Value -> Parser Generator
parseGeneratorKey _ "field" v = liftWith JSON.withText "field name"
  parseField v
parseGeneratorKey _ "string" v = liftWith JSON.withText "string literal"
  (return . GeneratorString) v
parseGeneratorKey _ "paste" (JSON.String s) = parseSubst s
parseGeneratorKey g "paste" v = liftWith JSON.withArray "paste components"
  (fmap GeneratorPaste . mapM (parseGenerator g) . V.toList) v
parseGeneratorKey g "handle" v =
  GeneratorMap (valuesGenerator . foldMap handleToID . values) <$> parseGenerator g v
parseGeneratorKey g "value" v = parseGenerator g v
parseGeneratorKey g k JSON.Null | Just m <- HMap.lookup k g = reWriter m -- macro with no arguments
parseGeneratorKey g k v | Just m <- HMap.lookup k g = liftWith JSON.withObject "generator arguments" -- macro with arguments
  (parseInField k . parseGeneratorMacro (HMap.delete k g) m) v
parseGeneratorKey _ k _ = fail $ "Unknown field generator: " ++ show k

parseRegex :: Monad m => T.Text -> m TU.Regex
parseRegex = either (fail . show) return . TU.regex' [TU.ErrorOnUnknownEscapes]

parseMatch :: Macros -> JSON.Value -> Parser (Value -> Generator)
parseMatch _ (JSON.String s) = do
  r <- parseRegex s
  return $ valuesGenerator . filter (isJust . TU.find r) . values
parseMatch g (JSON.Object o) = do
  c <- mapM parse $ HMap.toList o
  return $ foldMap (matches c) . values
  where
  parse (k, v) = do
    r <- parseRegex k
    let l = groups r
    s <- censor (`HSet.difference` HSet.fromMap (void l)) $ parseGenerator g v
    return (r, l, s)
  matches c v = foldMap (`match` v) c
  match (r,l,s) = foldMap (\m -> GeneratorWith (valuesGenerator . maybeToList . ($ m) <$> l) s) . TU.find r
  groups m = HMap.fromList
    $ ("&", TU.group 0)
    : ("`", TU.prefix 0)
    : ("'", TU.suffix 0)
    : map (\n -> (T.pack $ show n, TU.group n)) [0..TU.groupCount m]
parseMatch _ v = lift $ JSON.typeMismatch "match string or cases" v

-- |Parse a JSON object as a generator
-- Some fields are handled specially (handlers), while the rest are passed to 'parseGeneratorKey' and merged.
parseGeneratorObject :: Macros -> JSON.Object -> Parser Generator
parseGeneratorObject g = run
  [ ("join", withj $ \j v@(Value l) -> if null l then v else value (T.intercalate j l))
  , ("default", withp (parseGenerator g) $ \d (Value l) -> if null l then d else valuesGenerator l)
  , ("limit", withj $ \n -> Value . take n . values)
  , ("match", parseMatch g)
  , ("date", withj $ \fmt -> foldMap (fromMaybe mempty . parseTimeM True defaultTimeLocale fmt . T.unpack) . values)
  ]
  where
  run [] o = foldMapM (\(k,v) -> parseInField k $ parseGeneratorKey g k v) (HMap.toList o)
  run ((f, h) : r) o
    | Just x <- HMap.lookup f o = GeneratorMap <$> parseInField f (h x) <*> run r (HMap.delete f o)
    | otherwise = run r o
  withp :: (JSON.Value -> Parser a) -> (a -> Value -> Generator) -> JSON.Value -> Parser (Value -> Generator)
  withp p h x = h <$> p x
  withj :: JSON.FromJSON a => (a -> Value -> Value) -> JSON.Value -> Parser (Value -> Generator)
  withj = withp (lift . JSON.parseJSON) . (((valuesGenerator . values) .) .)

-- |Parse a generator, given the macros in scope
parseGenerator :: Macros -> JSON.Value -> Parser Generator
parseGenerator _ (JSON.String f)
  | not (T.null f) && T.all isFieldChar f = parseField f
parseGenerator _ (JSON.String s) = parseSubst s
parseGenerator _ JSON.Null = return $ mempty
parseGenerator g (JSON.Array l) = GeneratorList <$> mapM (parseGenerator g) (V.toList l)
parseGenerator g (JSON.Object o) = parseGeneratorObject g o
parseGenerator _ v = lift $ JSON.typeMismatch "field generator" v

instance JSON.FromJSON (Fields Generator) where
  parseJSON = unTWriter . parseGenerator mempty

-- |Parse a cross-walk configuration as a set of field generators, given a set of generator macros.
parseGenerators :: Macros -> JSON.Value -> JSON.Parser FieldGenerators
parseGenerators g = withObjectOrNull "field generators" $ unTWriter . HMap.traverseWithKey (\k -> parseInField k . parseGenerator g)

-- |Translate language codes according to ISO639, if possible
languageGenerator :: ISO639 -> Generator -> Generator
languageGenerator iso = GeneratorMap $ valuesGenerator . map (\l -> fromMaybe l $ lookupISO639 iso l) . values

-- |Adjust a single field generator
mapGenerator :: T.Text -> (Generator -> Generator) -> FieldGenerators -> FieldGenerators
mapGenerator k f = mapWriter $ first $ HMap.adjust f k
