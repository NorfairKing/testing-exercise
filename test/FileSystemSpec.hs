{-# LANGUAGE TypeApplications #-}

module FileSystemSpec where

import Prelude hiding (readFile, writeFile)

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import FileSystem

-- stack test --file-watch
-- writeFile :: String -> String -> FileSystem -> FileSystem
-- readFile :: String -> FileSystem -> Maybe String
spec :: Spec
spec = do
  describe "writeFile" $ do
    it "writes this example file to an empty file system" $
      writeFile "path" "contents" (FileSystem []) `shouldBe` (FileSystem [("path", "contents")])
    it "writes this example file to a non-empty file system" $
      writeFile "path2" "contents2" (FileSystem [("path1", "contents1")]) `shouldBe`
      (FileSystem [("path1", "contents1"), ("path2", "contents2")])
