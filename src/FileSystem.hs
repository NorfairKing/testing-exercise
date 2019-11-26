{-# LANGUAGE DeriveGeneric #-}

module FileSystem where

import GHC.Generics (Generic)

import Prelude hiding (readFile, writeFile)

import Data.Validity

newtype FileSystem =
  FileSystem {fileSystemFiles :: [(String, String)]}
  deriving (Show, Eq, Ord, Generic)

instance Validity FileSystem

writeFile :: String -> String -> FileSystem -> FileSystem
writeFile path contents (FileSystem fs) =
  FileSystem ((path, contents) : filter ((/= path) . fst) fs)

readFile :: String -> FileSystem -> Maybe String
readFile path (FileSystem fs) = lookup path fs
