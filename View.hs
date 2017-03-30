{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module View
  ( view
  ) where

import           Control.Monad (join, when)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import           Data.Function (on)
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (fromMaybe, isJust, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.URI (Query, encodePath)
import           Network.URI (parseURI, uriScheme)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Read (readMaybe)

import           Config
import           Document

htmlValue :: Value -> H.Html
htmlValue (Value []) = mempty
htmlValue (Value [s])
  | Just u <- parseURI (T.unpack s), uriScheme u `elem` ["http:", "https:", "ftp:"] =
    H.a H.! HA.href (H.stringValue $ show u) $ H.text s
  | otherwise = H.text s
htmlValue (Value l) = H.ol $ foldMap (H.li . htmlValue . value) l

htmlDocument :: Document -> H.Html
htmlDocument m =
  H.h3 (htmlValue $ getMetadata m "id")
  <> H.dl
    (HMap.foldrWithKey (\k v -> mappend $ H.dt (H.text k) <> H.dd (htmlValue v))
      mempty m)

htmlDocuments :: Documents -> H.Html
htmlDocuments = foldMap htmlDocument

view :: Config -> Maybe Collection -> Documents -> Bool -> Query -> H.Html
view conf coll d orig q =
  H.docTypeHtml $ H.body $ H.form H.! HA.method "get" H.! HA.action "/"
    $  H.label ("collection" <> (H.select H.! HA.name "collection"
      $ (H.option H.! HA.value mempty H.!? (isNothing coll, HA.selected "selected") $ "all")
      <> foldMap (\c ->
          let i = collectionKey c in
          H.option H.! HA.value (H.textValue i) H.!? (any (on (==) collectionKey $ c) coll, HA.selected "selected")
            $ H.text $ fromMaybe i $ collectionName c)
        (configCollections conf)))
    <> " " <> H.label (H.input H.! HA.type_ "number" H.! HA.name "count" H.! HA.min (numValue 1) H.! HA.max (numValue n) H.! HA.step (numValue 1) H.! HA.value (numValue count)
      <> "per page")
    <> " " <> H.input H.! HA.type_ "submit"
    <> foldMap (\c -> H.h2 (H.text $ fromMaybe (collectionKey c) $ collectionName c)) coll
    <> H.div
      ((H.a H.! HA.href (H.unsafeLazyByteStringValue $ BSB.toLazyByteString $ encodePath (foldMap (return . collectionKey) coll) [("json", Just "1")]) $ "json")
      <> " " <> H.toMarkup n <> " documents "
      <> if isJust coll then H.label
        $ H.input H.! HA.type_ "checkbox" H.! HA.name "orig" H.!? (orig, HA.checked "checked")
        <> "untransformed fields" else mempty)
    <> htmlDocuments l
    <> pagenav
  where
  n = V.length d
  np = (n + count - 1) `div` count
  l = V.take count $ V.drop (count * pred page) d
  pagenav = H.div $ H.label $ "page"
    <> (when (page > 1) $ navto 1)
    <> (when (page > 3) $ H.preEscapedString "&hellip;")
    <> (when (page > 2) $ navto (pred page))
    <> H.input H.! HA.type_ "number" H.! HA.name "page" H.! HA.min (numValue 1) H.! HA.max (numValue np) H.! HA.step (numValue 1) H.! HA.value (numValue page)
    <> (when (page < np-1) $ navto (succ page))
    <> (when (page < np-2) $ H.preEscapedString "&hellip;")
    <> (when (page < np) $ navto np)
  getq b k = fromMaybe b $ readMaybe . BSC.unpack =<< join (lookup k q)
  page = max 1 $ min np $ getq (getq 1 "page") "nav"
  count = max 1 $ getq 10 "count"
  numValue :: Int -> H.AttributeValue
  numValue = H.toValue
  navto i = H.input H.! HA.type_ "submit" H.! HA.name "nav" H.! HA.value (numValue i)
