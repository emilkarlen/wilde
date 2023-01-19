module TestResources.Testing.TestWithTmpDir where

import Data.ByteString
import Test.HUnit

import qualified TestResources.Testing.TmpDir as TmpDir

assertWithTmpDir
    :: TmpDir.DirPopulator
    -> (FilePath -> Assertion)
       -- ^ Given the path of the tmp dir, perform the test
    -> Assertion
assertWithTmpDir = TmpDir.withTmpDir

assertWithTmpFile
    :: String
       -- ^ tmp file name (without dir components)
    -> String
       -- ^ tmp file contents
    -> (FilePath -> Assertion)
       -- ^ action to run with the path of the tmp file
    -> Assertion
assertWithTmpFile = TmpDir.withTmpFile

assertWithTmpFile_b
    :: String
       -- ^ tmp file name (without dir components)
    -> ByteString
       -- ^ tmp file contents
    -> (FilePath -> Assertion)
       -- ^ action to run with the path of the tmp file
    -> Assertion
assertWithTmpFile_b = TmpDir.withTmpFile_b
