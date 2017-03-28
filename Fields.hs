{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Fields
  ( Generators
  , fieldGenerator
  , generateFields
  , generatorsFields
  , parseGenerators
  , languageGenerator
  ) where

import qualified Data.Aeson.Types as JSON
import           Data.Char (isAlpha)
import qualified Data.HashMap.Lazy as HMapL
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Vector as V

import           Util
import           ISO639
import           Document

-- |A 'Value' generator for a single metadata field
data Generator
  = GeneratorString T.Text -- ^fixed text value
  | GeneratorField T.Text -- ^copy input field
  | GeneratorMap
    { _generatorMap :: Value -> Value -- ^apply this function to the resulting value
    , _generator :: Generator
    }
  | GeneratorList [Generator] -- ^concatenate (`mconcat`) all the values
  | GeneratorPaste [Generator] -- ^join (`T.concat`) all the values, producing the cross-product
  | GeneratorOr
    { _generator :: Generator
    , _generatorOr :: Generator -- ^use if generator produces the empty value
    }
  | GeneratorWith
    { _generatorWith :: HMap.HashMap T.Text Generator -- ^assign local variables to replace input
    , _generator :: Generator
    }

-- |Generate a static string
instance IsString Generator where
  fromString = GeneratorString . fromString

fieldGenerator :: T.Text -> Generator
fieldGenerator = GeneratorField

-- |Merge generators using 'GeneratorList'
instance Monoid Generator where
  mempty = GeneratorList []
  mappend (GeneratorList a) (GeneratorList b) = GeneratorList (a <> b)
  mappend a@(GeneratorList _) b = a <> GeneratorList [b]
  mappend a b = GeneratorList [a] <> GeneratorList [b]

-- |Generate a single 'Value' given a set of input metadata values and a field 'Generator'
generate :: Metadata -> Generator -> Value
generate _ (GeneratorString x) = value x
generate m (GeneratorField f) = HMap.lookupDefault mempty f m
generate m (GeneratorMap f g) = f $ generate m g
generate m (GeneratorList l) = foldMap (generate m) l
generate m (GeneratorOr g d) = generate m g `valueOr` generate m d
generate _ (GeneratorPaste []) = value T.empty
generate m (GeneratorPaste [x]) = generate m x
generate m (GeneratorPaste (g:l)) = Value
  [ x <> y
  | x <- values $ generate m g
  , y <- values $ generate m (GeneratorPaste l)
  ]
generate m (GeneratorWith gm g) = generate (HMap.map (generate m) gm) g

-- |Collect the set of input fields consumed by a generator
generatorFields :: Generator -> HSet.HashSet T.Text
generatorFields (GeneratorString _) = HSet.empty
generatorFields (GeneratorList l) = foldMap generatorFields l
generatorFields (GeneratorField f) = HSet.singleton f
generatorFields (GeneratorMap _ g) = generatorFields g
generatorFields (GeneratorOr g d) = generatorFields g <> generatorFields d
generatorFields (GeneratorPaste l) = foldMap generatorFields l
generatorFields (GeneratorWith gm g) = foldMap generatorFields gm <> (generatorFields g `HSet.difference` HSet.fromMap (HMapL.map (const ()) gm))

-- |Translate language codes according to ISO639, if possible
languageGenerator :: ISO639 -> Generator -> Generator
languageGenerator iso = GeneratorMap $ mapValues (\l -> fromMaybe l $ lookupISO639 iso l)

-- |An entire metadata cross-walk, mapping a set of output fields to their 'Generator'
type Generators = HMap.HashMap T.Text Generator

-- |Translate metadata using a cross-walk
generateFields :: Generators -> Metadata -> Metadata
generateFields g m = HMap.map (generate m) g

-- |Collect the set of input fields consumed by generators
generatorsFields :: Generators -> HSet.HashSet T.Text
generatorsFields = foldMap generatorFields

-- |Parse a single field from a JSON object as a generator
parseGeneratorKey :: Generators -> T.Text -> JSON.Value -> JSON.Parser Generator
parseGeneratorKey _ "field" v =
  JSON.withText "field name" (return . GeneratorField) v
parseGeneratorKey _ "string" v =
  JSON.withText "string literal" (return . GeneratorString) v
parseGeneratorKey g "paste" v =
  JSON.withArray "paste components" (fmap GeneratorPaste . mapM (parseGenerator g) . V.toList) v
parseGeneratorKey g "handle" v =
  GeneratorMap (Value . foldMap handleToID . values) <$> parseGenerator g v
parseGeneratorKey g "value" v = parseGenerator g v
parseGeneratorKey g k JSON.Null | Just m <- HMap.lookup k g = return m -- macro with no arguments
parseGeneratorKey g k v | Just m <- HMap.lookup k g = -- macro with arguments
  JSON.withObject "generator arguments" (fmap (`GeneratorWith` m) . mapM (parseGenerator $ HMap.delete k g)) v
parseGeneratorKey _ k _ = fail $ "Unknown field generator: " ++ show k

-- |Parse a JSON object as a generator
-- Some fields are handled specially (handlers), while the rest are passed to 'parseGeneratorKey' and merged.
parseGeneratorObject :: Generators -> JSON.Object -> JSON.Parser Generator
parseGeneratorObject g = run handlers where
  run [] o = foldMapM (uncurry $ parseGeneratorKey g) (HMap.toList o)
  run ((f, h) : r) o
    | Just x <- HMap.lookup f o = h x =<< run r (HMap.delete f o)
    | otherwise = run r o
  handlers =
    [ ("join", gmap $ \j (Value l) -> if null l then Value l else value $ T.intercalate j l)
    , ("default", \j x -> GeneratorOr x <$> parseGenerator g j)
    , ("limit", gmap $ \n -> Value . take n . values)
    , ("lookup", gmap $ \c -> foldMap (getMetadata c) . values)
    , ("date", gmap $ \fmt -> foldMap (fromMaybe mempty . parseTimeM True defaultTimeLocale fmt . T.unpack) . values)
    ]
  gmap h j x = flip GeneratorMap x . h <$> JSON.parseJSON j

-- |Parse a generator, given the macros in scope
parseGenerator :: Generators -> JSON.Value -> JSON.Parser Generator
parseGenerator _ (JSON.String f)
  | Just (h, _) <- T.uncons f -- heuristics for valid field names
  , isAlpha h || h == '_' = return $ GeneratorField f
parseGenerator _ (JSON.String s) = return $ GeneratorString s
parseGenerator _ JSON.Null = return $ mempty
parseGenerator g (JSON.Array l) = GeneratorList <$> mapM (parseGenerator g) (V.toList l)
parseGenerator g (JSON.Object o) = parseGeneratorObject g o
parseGenerator _ v = JSON.typeMismatch "field generator" v

instance JSON.FromJSON Generator where
  parseJSON = parseGenerator mempty

-- |Parse a cross-walk configuration as a set of field generators, given a set of generator macros.
parseGenerators :: Generators -> JSON.Value -> JSON.Parser Generators
parseGenerators g v = withObjectOrNull "field generators" (mapM $ parseGenerator g) v
