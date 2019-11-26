{-# LANGUAGE TypeApplications #-}

module FileSystemSpec where

import Prelude hiding (readFile, writeFile)

import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import FileSystem

-- stack test --file-watch
-- writeFile :: String -> String -> FileSystem -> FileSystem
-- readFile :: String -> FileSystem -> Maybe String
instance Arbitrary FileSystem where
  arbitrary = FileSystem <$> arbitrary

instance GenUnchecked FileSystem

instance GenValid FileSystem

spec :: Spec
spec = do
  xdescribe "off" $ do
    ordSpecOnValid @FileSystem
    describe "writeFile" $ do
      it "writes this example file to an empty file system" $
        writeFile "path" "contents" (FileSystem []) `shouldBe` (FileSystem [("path", "contents")])
      it "writes this example file to a non-empty file system" $
        writeFile "path2" "contents2" (FileSystem [("path1", "contents1")]) `shouldBe`
        (FileSystem [("path1", "contents1"), ("path2", "contents2")])
      it "is idempotent" $ do
        property $ \fs ->
          property $ \path ->
            property $ \contents ->
              let fs1 = writeFile path contents fs
               in writeFile path contents fs1 `shouldBe` fs1
      it "is idempotent" $ do
        property $ \fs ->
          property $ \path ->
            property $ \contents ->
              let fs1 = writeFile path contents fs
               in writeFile path contents fs1 `shouldBe` fs1
      it "writing a file twice is the same as writing it once" $
        forAllValid $ \(path, contents) -> idempotentOnValid (writeFile path contents)
    describe "readFile" $ do
      it "reads the file that was just written" $
        property $ \fs ->
          property $ \path ->
            property $ \contents ->
              readFile path (writeFile path contents fs) `shouldBe` Just contents
      it "writing a file that already existed with the same contents, doesn't change anything" $
        forAll (arbitrary `suchThat` (not . null . fileSystemFiles)) $ \fs ->
          let (FileSystem files) = fs
           in forAll (elements files) $ \(path, contents) ->
                writeFile path contents fs `shouldBe` fs
  describe "easier properties" $ do
    it "reversing a list twice is the same as not reversing it" $
      property $ \ls -> reverse (reverse ls) `shouldBe` (ls :: [Int])
    it "two times a number is the same as the number plus itself" $
      forAllValid $ \d -> 2 * (d :: Double) `shouldBe` (d + d)
    it "two times a number is the same as the number plus itself" $
      forAllValid $ \d -> 2 * (d :: Double) `shouldBe` (d + d)
    it "encode/decode utf16" $ forAllValid $ \s -> T.unpack (T.pack s) `shouldBe` s


