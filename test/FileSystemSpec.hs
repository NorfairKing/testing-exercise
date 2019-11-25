{-# LANGUAGE TypeApplications #-}

module FileSystemSpec where

import Prelude hiding (readFile, writeFile)

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import FileSystem

instance Arbitrary FileSystem where
  arbitrary = FileSystem <$> arbitrary

instance GenUnchecked FileSystem

instance GenValid FileSystem

spec :: Spec
spec = do
  eqSpecOnValid @FileSystem
  genValidSpec @FileSystem
  describe "writeFile" $ do
    it "succesfully writes this first file" $
      writeFile "path" "contents" (FileSystem []) `shouldBe` (FileSystem [("path", "contents")])
    -- First unit test
    -- Second unit test
    it "succesfully writes this second file" $
      writeFile "path2" "contents2" (FileSystem [("path1", "contents1")]) `shouldBe`
      (FileSystem [("path1", "contents1"), ("path2", "contents2")])
    -- Property testing using quick check (arbitrary instance)
    it "writing a file twice is the same as writing it once" $
      property $ \fs ->
        property $ \(path, contents) ->
          writeFile path contents (writeFile path contents fs) `shouldBe` writeFile path contents fs
    -- Property testing with genvalidity
    it "writing a file twice is the same as writing it once" $
      forAllValid $ \fs ->
        forAllValid $ \(path, contents) ->
          writeFile path contents (writeFile path contents fs) `shouldBe` writeFile path contents fs
    -- Property test using genvalidity-property
    it "writing a file twice is the same as writing it once" $
      forAllValid $ \(path, contents) -> idempotentOnValid (writeFile path contents)
    --
    it "writing a file that already existed with the same contents, doesn't change anything" $
      forAll (genValid `suchThat` (not . null . fileSystemFiles)) $ \fs ->
        let (FileSystem files) = fs
         in forAll (elements files) $ \(path, contents) -> writeFile path contents fs `shouldBe` fs
  describe "readFile/writeFile" $ do
    it "Reading a file that was just written retrieves the contents that were just written" $ do
      forAllValid $ \(path, contents) ->
        forAllValid $ \fs -> readFile path (writeFile path contents fs) `shouldBe` Just contents
