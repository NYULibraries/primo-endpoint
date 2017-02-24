{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Fields
  ( Generators
  , generateFields
  , parseGenerators
  ) where

import qualified Data.Aeson.Types as JSON
import           Data.Char (isAlpha)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V

import           Util
import           Document

data Generator
  = GeneratorString T.Text
  | GeneratorField T.Text
  | GeneratorList [Generator]
  | GeneratorPaste [Generator] -- cross-product
  | GeneratorOr
    { _generator :: Generator
    , _generatorOr :: Generator -- if generator is empty
    }
  | GeneratorWith
    { _generatorWith :: HM.HashMap T.Text Generator -- local variables
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
generate m (GeneratorField f) = HM.lookupDefault mempty f m
generate m (GeneratorOr g d) = generate m g `valueOr` generate m d
generate _ (GeneratorPaste []) = Value [T.empty]
generate m (GeneratorPaste [x]) = generate m x
generate m (GeneratorPaste (g:l)) = Value
  [ x <> y
  | x <- values $ generate m g
  , y <- values $ generate m (GeneratorPaste l)
  ]
generate m (GeneratorWith gm g) = generate (HM.map (generate m) gm) g

type Generators = HM.HashMap T.Text Generator

generateFields :: Generators -> Metadata -> Metadata
generateFields g m = HM.map (generate m) g

parseGeneratorKey :: Generators -> T.Text -> JSON.Value -> JSON.Parser Generator
parseGeneratorKey _ "field" v =
  JSON.withText "field name" (return . GeneratorField) v
parseGeneratorKey _ "string" v =
  JSON.withText "string literal" (return . GeneratorString) v
parseGeneratorKey g "paste" v =
  JSON.withArray "paste components" (fmap GeneratorPaste . mapM (parseGenerator g) . V.toList) v
parseGeneratorKey g k JSON.Null | Just m <- HM.lookup k g = return m
parseGeneratorKey g k v | Just m <- HM.lookup k g =
  JSON.withObject "generator arguments" (fmap (`GeneratorWith` m) . mapM (parseGenerator $ HM.delete k g)) v
parseGeneratorKey _ k _ = fail $ "Unknown field generator: " ++ show k

parseGenerator :: Generators -> JSON.Value -> JSON.Parser Generator
parseGenerator _ (JSON.String f)
  | Just (h, _) <- T.uncons f
  , isAlpha h = return $ GeneratorField f
parseGenerator _ (JSON.String s) = return $ GeneratorString s
parseGenerator _ JSON.Null = return $ mempty
parseGenerator g (JSON.Array l) = GeneratorList <$> mapM (parseGenerator g) (V.toList l)
parseGenerator g (JSON.Object o) = do
  maybe return (\d x -> GeneratorOr x <$> parseGenerator g d) (HM.lookup "default" o)
    =<< foldMapM (uncurry $ parseGeneratorKey g) (HM.toList $ HM.delete "default" o)
parseGenerator _ v = JSON.typeMismatch "field generator" v

instance JSON.FromJSON Generator where
  parseJSON = parseGenerator mempty

parseGenerators :: Generators -> JSON.Value -> JSON.Parser Generators
parseGenerators g v = withObjectOrNull "field generators" (mapM $ parseGenerator g) v
