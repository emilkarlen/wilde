{-# LANGUAGE OverloadedStrings #-}
module WildeTest.Driver.WaiServer.FilePathHandling.TestResources where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import Test.HUnit

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling as Sut


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


compWExt :: T.Text -> T.Text -> T.Text
compWExt head ext = head <> "." <> ext

tcs_wLastComponents :: [T.Text] -> (Sut.RequestPath -> Assertion) -> Test
tcs_wLastComponents lastComponents assertionForPath =
  TestList
  [
    "1 comp" ~: TestCase $ assertionForPath lastComponents
  , "2 comp" ~: TestCase $ assertionForPath (["1st-comp"] <> lastComponents)
  , "3 comp" ~: TestCase $ assertionForPath (["comp1", "comp2"] <> lastComponents)
  ]
