{-# LANGUAGE OverloadedStrings #-}

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))

import           Test.Hspec

import Cache
import ISO639

main :: IO ()
main = do
  cdir <- defaultCacheDir
  createDirectoryIfMissing True cdir
  hspec $ do
    describe "ISO639" $ do
      iso <- runIO $ loadISO639 (cdir </> "iso639")
      it "translates English" $ do
        lookupISO639 iso "eng" `shouldBe` Just "English"
        lookupISO639 iso "EN" `shouldBe` Just "English"
      it "translates Zza" $ do
        lookupISO639 iso "Zza" `shouldBe` Just "Zaza"
      it "does not translate zzz" $ do
        lookupISO639 iso "zzz" `shouldBe` Nothing
