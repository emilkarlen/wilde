{-# LANGUAGE OverloadedStrings #-}
module WildeTest.Driver.WaiServer.FilePathHandling.ResolveMimeType
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import Test.HUnit

import           WildeTest.Driver.WaiServer.FilePathHandling.TestResources

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling as Sut

import qualified TestResources.AssertUtils as Asrt


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  TestList
    [
       "mime type is mapped"     ~: resolveMimeType_mapped
    ,  "mime type is not mapped" ~: resolveMimeType_notMapped
    ]

resolveMimeType_mapped :: Test
resolveMimeType_mapped =
  TestList
  [
    "1 extension" ~:
    casesWithAndWithoutInitialDotsOfHead
    (compWExt "head" ext1)
    mimeType1

  , "WHEN 2 extension separators THEN last ext should be used" ~:
    casesWithAndWithoutInitialDotsOfHead
    (compWExt (compWExt "the-head" ext1) ext2) $
    mimeType2
  ]
  where
    casesWithAndWithoutInitialDotsOfHead :: T.Text -> Sut.MimeType -> Test
    casesWithAndWithoutInitialDotsOfHead lastPathCompWoInitDot expectedMimeType =
      TestList
      [
        "head wo dot-prefix" ~:
        tcs_wLastComponents
        [lastPathCompWoInitDot] $
        isMappedTo expectedMimeType
      
      , "head begins w 1 dot" ~:
        tcs_wLastComponents
        ["." <> lastPathCompWoInitDot] $
        isMappedTo expectedMimeType
      
      , "head begins w 2 dots" ~:
        tcs_wLastComponents
        [".." <> lastPathCompWoInitDot] $
        isMappedTo expectedMimeType

      ]
    ext1, ext2 :: T.Text
    ext1 = "ext1"
    ext2 = "ext2"

    mimeType1, mimeType2 :: Sut.MimeType
    mimeType1 = "the mime type 1"
    mimeType2 = "the mime type 2"

    mtm :: Sut.MimeTypeMapping
    mtm = M.fromList [(ext1, mimeType1), (ext2, mimeType2)]

    isMappedTo :: Sut.MimeType -> Sut.RequestPath -> Assertion
    isMappedTo expectedMimeType = asrtMapped mtm expectedMimeType 

resolveMimeType_notMapped :: Test
resolveMimeType_notMapped =
  TestList
  [
    "mapped extension appears in non-last path component" ~:
    tcs_wLastComponents
    [compWExt "a" ext_mapped, compWExt "b" ext_notMapped]
    isNotMapped

  , "mapped extension appears directly after initial dot" ~:
    tcs_wLastComponents
    [compWExt ("." <> ext_mapped) ext_notMapped]
    isNotMapped

  , "mapped extension appears directly after non-initial dot" ~:
    tcs_wLastComponents
    [compWExt (compWExt "head" ext_mapped) ext_notMapped]
    isNotMapped
  , "simple head.ext" ~:
    TestList
    [
      "empty mapping" ~:
      tcs_wLastComponents
      [compWExt "head" ext_notMapped]
      isNotMapped_emptyMapping

    , "non-empty mapping" ~:
      tcs_wLastComponents
      [compWExt "head" ext_notMapped]
      isNotMapped
    ]
  ]
  where
    ext_mapped, ext_notMapped :: T.Text
    ext_mapped    = "ext-mapped"
    ext_notMapped = "ext-not-mapped"

    mimeType_mapped :: Sut.MimeType
    mimeType_mapped = "the mime type"

    mtm_wMappedExt :: Sut.MimeTypeMapping
    mtm_wMappedExt = M.singleton ext_mapped mimeType_mapped

    isNotMapped :: Sut.RequestPath -> Assertion
    isNotMapped = asrtNotMapped mtm_wMappedExt

    isNotMapped_emptyMapping :: Sut.RequestPath -> Assertion
    isNotMapped_emptyMapping = asrtNotMapped mtm_wMappedExt

asrtMapped :: Sut.MimeTypeMapping -> Sut.MimeType -> Sut.RequestPath -> Assertion
asrtMapped mtm expected path =
  assertEqual "" (Right expected) $
  Sut.resolveMimeTypeFromValidPath mtm path

asrtNotMapped :: Sut.MimeTypeMapping -> Sut.RequestPath -> Assertion
asrtNotMapped mtm path = Asrt.isLeft $ Sut.resolveMimeTypeFromValidPath mtm path
