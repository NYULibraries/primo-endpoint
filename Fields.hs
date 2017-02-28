{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Fields
  ( Generators
  , generateFields
  , generatorsFields
  , parseGenerators
  ) where

import qualified Data.Aeson.Types as JSON
import           Data.Char (isAlpha)
import qualified Data.HashMap.Lazy as HMapL
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Vector as V

import           Util
import           Document

data Generator
  = GeneratorString T.Text
  | GeneratorField T.Text
  | GeneratorMap
    { _generatorMap :: T.Text -> Value
    , _generator :: Generator
    }
  | GeneratorList [Generator]
  | GeneratorPaste [Generator] -- cross-product
  | GeneratorOr
    { _generator :: Generator
    , _generatorOr :: Generator -- if generator is empty
    }
  | GeneratorWith
    { _generatorWith :: HMap.HashMap T.Text Generator -- local variables
    , _generator :: Generator
    }

instance Monoid Generator where
  mempty = GeneratorList []
  mappend (GeneratorList a) (GeneratorList b) = GeneratorList (a <> b)
  mappend a@(GeneratorList _) b = a <> GeneratorList [b]
  mappend a b = GeneratorList [a] <> GeneratorList [b]

generate :: Metadata -> Generator -> Value
generate _ (GeneratorString x) = Value [x]
generate m (GeneratorList l) = foldMap (generate m) l
generate m (GeneratorField f) = HMap.lookupDefault mempty f m
generate m (GeneratorMap f g) = foldMap f $ values $ generate m g
generate m (GeneratorOr g d) = generate m g `valueOr` generate m d
generate _ (GeneratorPaste []) = Value [T.empty]
generate m (GeneratorPaste [x]) = generate m x
generate m (GeneratorPaste (g:l)) = Value
  [ x <> y
  | x <- values $ generate m g
  , y <- values $ generate m (GeneratorPaste l)
  ]
generate m (GeneratorWith gm g) = generate (HMap.map (generate m) gm) g

generatorFields :: Generator -> HSet.HashSet T.Text
generatorFields (GeneratorString _) = HSet.empty
generatorFields (GeneratorList l) = foldMap generatorFields l
generatorFields (GeneratorField f) = HSet.singleton f
generatorFields (GeneratorMap _ g) = generatorFields g
generatorFields (GeneratorOr g d) = generatorFields g <> generatorFields d
generatorFields (GeneratorPaste l) = foldMap generatorFields l
generatorFields (GeneratorWith gm g) = foldMap generatorFields gm <> (generatorFields g `HSet.difference` HSet.fromMap (HMapL.map (const ()) gm))

type Generators = HMap.HashMap T.Text Generator

generateFields :: Generators -> Metadata -> Metadata
generateFields g m = HMap.map (generate m) g

generatorsFields :: Generators -> HSet.HashSet T.Text
generatorsFields = foldMap generatorFields

parseGeneratorKey :: Generators -> T.Text -> JSON.Value -> JSON.Parser Generator
parseGeneratorKey _ "field" v =
  JSON.withText "field name" (return . GeneratorField) v
parseGeneratorKey _ "string" v =
  JSON.withText "string literal" (return . GeneratorString) v
parseGeneratorKey g "paste" v =
  JSON.withArray "paste components" (fmap GeneratorPaste . mapM (parseGenerator g) . V.toList) v
parseGeneratorKey g "date" v =
  JSON.withObject "date parser" (\o -> do
    fmt <- o JSON..: "format"
    val <- parseGenerator g =<< o JSON..: "value"
    return $ GeneratorMap
      (fromMaybe mempty . parseTimeM True defaultTimeLocale fmt . T.unpack)
      val)
    v
parseGeneratorKey g k JSON.Null | Just m <- HMap.lookup k g = return m
parseGeneratorKey g k v | Just m <- HMap.lookup k g =
  JSON.withObject "generator arguments" (fmap (`GeneratorWith` m) . mapM (parseGenerator $ HMap.delete k g)) v
parseGeneratorKey _ k _ = fail $ "Unknown field generator: " ++ show k

parseGenerator :: Generators -> JSON.Value -> JSON.Parser Generator
parseGenerator _ (JSON.String f)
  | Just (h, _) <- T.uncons f
  , isAlpha h = return $ GeneratorField f
parseGenerator _ (JSON.String s) = return $ GeneratorString s
parseGenerator _ JSON.Null = return $ mempty
parseGenerator g (JSON.Array l) = GeneratorList <$> mapM (parseGenerator g) (V.toList l)
parseGenerator g (JSON.Object o) = do
  maybe return (\d x -> GeneratorOr x <$> parseGenerator g d) (HMap.lookup "default" o)
    =<< foldMapM (uncurry $ parseGeneratorKey g) (HMap.toList $ HMap.delete "default" o)
parseGenerator _ v = JSON.typeMismatch "field generator" v

instance JSON.FromJSON Generator where
  parseJSON = parseGenerator mempty

parseGenerators :: Generators -> JSON.Value -> JSON.Parser Generators
parseGenerators g v = withObjectOrNull "field generators" (mapM $ parseGenerator g) v
