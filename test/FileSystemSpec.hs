{-# LANGUAGE TypeApplications #-}

module FileSystemSpec where

import Prelude hiding (readFile, writeFile)

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import FileSystem

instance GenUnchecked FileSystem

instance GenValid FileSystem

spec :: Spec
spec = do
  eqSpecOnValid @FileSystem
  genValidSpec @FileSystem
  describe "writeFile" $ do
    it "writing a file twice is the same as writing it once" $
      forAllValid $ \(path, contents) -> idempotentOnValid (writeFile path contents)
    it
      "writing a file that already existed with the same contents, doesn't change anything" $
      forAll
      (genValid `suchThat` (not . null . fileSystemFiles)) $ \fs ->
      let (FileSystem files) = fs
       in forAll (elements files) $ \(path, contents) -> writeFile path contents fs `shouldBe` fs
  describe "readFile/writeFile" $ do
    it "Reading a file that was just written retrieves the contents that were just written" $ do
      forAllValid $ \(path, contents) ->
        forAllValid $ \fs -> readFile path (writeFile path contents fs) `shouldBe` Just contents
