{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module View
  ( view
  ) where

import           Control.Arrow (left)
import           Control.Monad (join)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.HTTP.Types.URI (Query)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Read (readMaybe)

import           Util
import           Config
import           Document

htmlValue :: Value -> H.Html
htmlValue (Value l) = H.ol $ foldMap (H.li . H.text) l

htmlDocument :: Document -> H.Html
htmlDocument Document{..} =
  H.h3 (H.text documentID)
  <> H.dl
    (H.dt "collection" <> H.dd (H.text documentCollection)
    <> HMap.foldrWithKey (\k v -> mappend $ H.dt (H.text k) <> H.dd (htmlValue v))
      mempty documentMetadata)

htmlDocuments :: Documents -> H.Html
htmlDocuments = foldMap htmlDocument

view :: Config -> Maybe T.Text -> FilePath -> Query -> IO H.Html
view conf collid f q = do
  d <- fromDoesNotExist BSLC.empty $ BSLC.readFile f
  let j = maybe (Left d) Right $ JSON.decode d
      n = either (const 0) V.length j
      np = (n + count - 1) `div` count
      l = mapM (left BSLC.pack . JSON.parseEither JSON.parseJSON) . V.take count . V.drop (count * pred page) =<< j
  return $ H.docTypeHtml $ H.body $
    (H.form H.! HA.method "get" H.! HA.action "/" $
      (H.select H.! HA.name "collection")
        (collopt Nothing "all"
        <> HMap.foldrWithKey (\k v -> mappend $
            collopt (Just k) $ fromMaybe k $ collectionName v)
          mempty (configCollections conf))
      -- <> (if page == 1 then mempty else H.input H.! HA.type_ "submit" H.! HA.name "page" H.! HA.value "1")
      <> H.input H.! HA.type_ "number" H.! HA.name "page" H.! HA.min (numValue 1) H.! HA.max (numValue np) H.! HA.step (numValue 1) H.! HA.value (numValue page)
      <> H.input H.! HA.type_ "number" H.! HA.name "count" H.! HA.min (numValue 1) H.! HA.step (numValue 1) H.! HA.value (numValue count)
      <> H.input H.! HA.type_ "submit")
    <> either (H.string . BSLC.unpack) htmlDocuments l
  where
  getq d k = fromMaybe d $ readMaybe . BSC.unpack =<< join (lookup k q)
  page = getq 1 "page"
  count = max 1 $ getq 10 "count"
  collopt i n = H.option H.! HA.value (foldMap H.textValue i) H.!? (collid == i, HA.selected "selected") $ H.text n
  numValue = H.stringValue . show
