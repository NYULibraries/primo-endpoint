{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module View
  ( view
  ) where

import           Control.Arrow (left)
import           Control.Monad (join, when)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.URI (Query, encodePath)
import           Network.URI (parseURI)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Read (readMaybe)

import           Util
import           Config
import           Document

htmlValue :: Value -> H.Html
htmlValue (Value []) = mempty
htmlValue (Value [s])
  | Just u <- parseURI (T.unpack s) = H.a H.! HA.href (H.stringValue $ show u) $ H.text s
  | otherwise = H.text s
htmlValue (Value l) = H.ol $ foldMap (H.li . htmlValue . value) l

htmlDocument :: Document -> H.Html
htmlDocument Document{..} =
  H.h3 (H.text documentID)
  <> H.dl
    (H.dt "collection" <> H.dd (H.text documentCollection)
    <> HMap.foldrWithKey (\k v -> mappend $ H.dt (H.text k) <> H.dd (htmlValue v))
      mempty documentMetadata)

htmlDocuments :: Documents -> H.Html
htmlDocuments = foldMap htmlDocument

view :: Config -> Collection -> Query -> IO H.Html
view conf coll q = do
  d <- fromDoesNotExist BSLC.empty $ BSLC.readFile $ collectionCache coll
  let j = maybe (Left d) Right $ JSON.decode d
      n = either (const 0) V.length j
      np = (n + count - 1) `div` count
      l = mapM (left BSLC.pack . JSON.parseEither JSON.parseJSON) . V.take count . V.drop (count * pred page) =<< j
      pagenav = H.div $ H.label $ "page"
        <> (when (page > 1) $ navto 1)
        <> (when (page > 3) $ H.preEscapedString "&hellip;")
        <> (when (page > 2) $ navto (pred page))
        <> H.input H.! HA.type_ "number" H.! HA.name "page" H.! HA.min (numValue 1) H.! HA.max (numValue np) H.! HA.step (numValue 1) H.! HA.value (numValue page)
        <> (when (page < np-1) $ navto (succ page))
        <> (when (page < np-2) $ H.preEscapedString "&hellip;")
        <> (when (page < np) $ navto np)
  return $ H.docTypeHtml $ H.body $ H.form H.! HA.method "get" H.! HA.action "/"
    $  H.label ("collection" <> (H.select H.! HA.name "collection")
      (foldMap (\c ->
          let i = collectionId c in
          H.option H.! HA.value (H.textValue i) H.!? (collectionKey coll == collectionKey c, HA.selected "selected")
            $ H.text $ fromMaybe i $ collectionName c)
        $ allCollections conf))
    <> " " <> H.label (H.input H.! HA.type_ "number" H.! HA.name "count" H.! HA.min (numValue 1) H.! HA.max (numValue n) H.! HA.step (numValue 1) H.! HA.value (numValue count)
      <> "per page")
    <> " " <> H.input H.! HA.type_ "submit"
    <> H.h2 (H.text $ fromMaybe (collectionId coll) $ collectionName coll)
    <> " " <> (H.a H.! HA.href (H.unsafeLazyByteStringValue $ BSB.toLazyByteString $ encodePath (collectionKey coll) [("json", Just "1")])) "json"
    <> " " <> H.toMarkup n <> " documents"
    <> pagenav
    <> either (H.string . BSLC.unpack) htmlDocuments l
    <> pagenav
  where
  getq d k = fromMaybe d $ readMaybe . BSC.unpack =<< join (lookup k q)
  page = getq (getq 1 "page") "nav"
  count = max 1 $ getq 10 "count"
  numValue :: Int -> H.AttributeValue
  numValue = H.toValue
  navto n = H.input H.! HA.type_ "submit" H.! HA.name "nav" H.! HA.value (numValue n)
