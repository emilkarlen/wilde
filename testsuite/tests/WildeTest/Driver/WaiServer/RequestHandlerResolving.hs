{-# LANGUAGE OverloadedStrings #-}

module WildeTest.Driver.WaiServer.RequestHandlerResolving
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Text as T
import qualified Data.Map as M
import Test.HUnit

import Wilde.Driver.Application.WaiServer.RequestHandling.Main.RequestTypeResolving as Sut


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  TestLabel "resolve" $
  TestList
  [
    setupW_rootServices_noFiles
  , setupW_rootServices_nonRootFile
  , setupW_nonRootServices_rootFile
  , setupW_nonRootServices_nonRootFile
  , setupW_commonNonEmptyPrefixPart_servicesHasLongerPrefix
  , setupW_commonNonEmptyPrefixPart_fileHasLongerPrefix
  , setupW_filesWcommonNonEmptyPrefixPart
  ]

setupW_rootServices_noFiles :: Test
setupW_rootServices_noFiles =
  TestLabel "setupW_rootServices_noFiles" $
  mappedRoot setup Sut.Service "non-prefix-comp"

  where
    setup = Sut.PathPrefixesSetup
      {
        Sut.files    = M.empty
      , Sut.services = []
      }

setupW_rootServices_nonRootFile :: Test
setupW_rootServices_nonRootFile =
  TestLabel "setupW_rootServices_nonRootFile" $
  TestList [
     mappedRoot    setup  Sut.Service                            otherComp
   , mappedNonRoot setup (Sut.File fileSystemPfx, [filePfxComp]) otherComp
  ]
  where
    filePfxComp :: T.Text
    filePfxComp = "file-comp"
    fileSystemPfx :: String
    fileSystemPfx = "file-system-prefix"
    otherComp :: T.Text
    otherComp = "other"
    setup = Sut.PathPrefixesSetup
      {
        Sut.files    = M.singleton [filePfxComp] fileSystemPfx
      , Sut.services = []
      }


setupW_nonRootServices_rootFile :: Test
setupW_nonRootServices_rootFile =
  TestLabel "setupW_nonRootServices_rootFile" $
  TestList [
     mappedRoot    setup (Sut.File fileSystemPfx)        otherComp
   , mappedNonRoot setup (Sut.Service, [servicePfxComp]) otherComp
  ]
  where
    servicePfxComp :: T.Text
    servicePfxComp = "service-comp"
    fileSystemPfx :: String
    fileSystemPfx = "file-system-prefix"
    otherComp :: T.Text
    otherComp = "other"
    setup = Sut.PathPrefixesSetup
      {
        Sut.files    = M.singleton [] fileSystemPfx
      , Sut.services = [servicePfxComp]
      }

setupW_nonRootServices_nonRootFile :: Test
setupW_nonRootServices_nonRootFile =
  TestLabel "setupW_nonRootServices_nonRootFile" $
  TestList [
     mappedNonRoot setup (Sut.Service,            [servicePfxComp]) otherComp
   , mappedNonRoot setup (Sut.File fileSystemPfx, [filePfxComp])    otherComp
   , nonMappedRoot setup otherComp
  ]
  where
    filePfxComp :: T.Text
    filePfxComp = "file-prefix-comp"
    fileSystemPfx :: String
    fileSystemPfx = "file-system-prefix"
    servicePfxComp :: T.Text
    servicePfxComp = "service-prefix-comp"
    otherComp :: T.Text
    otherComp = "other"
    setup = Sut.PathPrefixesSetup
      {
        Sut.files    = M.singleton [filePfxComp] fileSystemPfx
      , Sut.services = [servicePfxComp]
      }

setupW_commonNonEmptyPrefixPart_servicesHasLongerPrefix :: Test
setupW_commonNonEmptyPrefixPart_servicesHasLongerPrefix =
  TestLabel "setupW_commonNonEmptyPrefixPart_servicesHasLongerPrefix" $
  TestList [
     mappedNonRoot setup (Sut.Service,            [commonComp1, serviceComp2]) otherComp
   , mappedNonRoot setup (Sut.File fileSystemPfx, [commonComp1])               otherComp
   , nonMappedRoot setup                                                       otherComp
  ]
  where
    commonComp1 :: T.Text
    commonComp1 = "common1"
    serviceComp2 :: T.Text
    serviceComp2 = "service-comp2"
    fileSystemPfx :: String
    fileSystemPfx = "file-system-prefix"
    otherComp :: T.Text
    otherComp = "other"
    setup = Sut.PathPrefixesSetup
      {
        Sut.files    = M.singleton [commonComp1] fileSystemPfx
      , Sut.services = [commonComp1, serviceComp2]
      }

setupW_commonNonEmptyPrefixPart_fileHasLongerPrefix :: Test
setupW_commonNonEmptyPrefixPart_fileHasLongerPrefix =
  TestLabel "setupW_commonNonEmptyPrefixPart_fileHasLongerPrefix" $
  TestList [
     mappedNonRoot setup (Sut.Service,            [commonComp1])               otherComp
   , mappedNonRoot setup (Sut.File fileSystemPfx, [commonComp1, fileComp2])    otherComp
   , nonMappedRoot setup                                                       otherComp
  ]
  where
    commonComp1 :: T.Text
    commonComp1 = "common1"
    fileComp2 :: T.Text
    fileComp2 = "file-comp2"
    fileSystemPfx :: String
    fileSystemPfx = "file-system-prefix"
    otherComp :: T.Text
    otherComp = "other"
    setup = Sut.PathPrefixesSetup
      {
        Sut.files    = M.singleton [commonComp1, fileComp2] fileSystemPfx
      , Sut.services = [commonComp1]
      }

setupW_filesWcommonNonEmptyPrefixPart :: Test
setupW_filesWcommonNonEmptyPrefixPart =
  TestLabel "setupW_filesWcommonNonEmptyPrefixPart" $
  TestList [
     mappedNonRoot setup (Sut.File fileSystemPfx1, [commonComp1])               otherComp
   , mappedNonRoot setup (Sut.File fileSystemPfx2, [commonComp1, fileComp_2_2]) otherComp
   , mappedNonRoot setup (Sut.File fileSystemPfx3, [commonComp1, fileComp_2_3]) otherComp
   , mappedRoot    setup  Sut.Service                                           otherComp
  ]
  where
    commonComp1 :: T.Text
    commonComp1 = "common"
    fileComp_2_2 :: T.Text
    fileComp_2_2 = "file-comp-2"
    fileComp_2_3 :: T.Text
    fileComp_2_3 = "file-comp-3"
    fileSystemPfx1 :: String
    fileSystemPfx1 = "file-system-prefix-1"
    fileSystemPfx2 :: String
    fileSystemPfx2 = "file-system-prefix-2"
    fileSystemPfx3 :: String
    fileSystemPfx3 = "file-system-prefix-3"
    otherComp :: T.Text
    otherComp = "other"
    setup = Sut.PathPrefixesSetup
      {
        Sut.files    = M.fromList [([commonComp1],               fileSystemPfx1),
                                   ([commonComp1, fileComp_2_2], fileSystemPfx2),
                                   ([commonComp1, fileComp_2_3], fileSystemPfx3)]
      , Sut.services = []
      }

checkForSetup
  :: Sut.PathPrefixesSetup
  -> [(String, Sut.RequestPath, Sut.Result)] -- ^ cases (descr, actual, expected)
  -> Test
checkForSetup setup cases =
  TestList $ map checkCase cases
  where
    checkCase :: (String, Sut.RequestPath, Sut.Result) -> Test
    checkCase (descr, actual, expected) =
      check setup expected descr actual

mappedRoot :: Sut.PathPrefixesSetup -> Sut.RequestType -> T.Text -> Test
mappedRoot setup expRt nonPrefixPathComp =
   TestLabel "mapped root path" $
   TestList [
     "path = []" ~:
     tc setup (Just (expRt, []))                       []

   , "path = prefix + [non-prefix]" ~:
     tc setup (Just (expRt, [nonPrefixPathComp]))      [nonPrefixPathComp]

   , "path = [non-prefix, empty]" ~:
      tc setup (Just (expRt, [nonPrefixPathComp, ""])) (nonPrefixPathComp : [""])

   , "path = [empty]" ~:
      tc setup (Just (expRt, [""]))                    [""]

   , "path = [empty, empty]" ~:
      tc setup (Just (expRt, ["", ""]))                ["", ""]
   ]

mappedNonRoot :: Sut.PathPrefixesSetup -> (Sut.RequestType, Sut.RequestPath) -> T.Text -> Test
mappedNonRoot setup (expRt, prefixForExpRt) otherPathComp =
   TestLabel "mapped non-root path" $
   TestList [
     "path = prefix" ~:
     tc setup (Just (expRt, []))              prefixForExpRt

   , "path = prefix + [other]" ~:
     tc setup (Just (expRt, [otherPathComp])) (prefixForExpRt ++ [otherPathComp])

   , "path = prefix + [empty]" ~:
      tc setup (Just (expRt, [""]))           (prefixForExpRt ++ [""])
   ]

nonMappedRoot :: Sut.PathPrefixesSetup -> T.Text -> Test
nonMappedRoot setup nonPrefixPathComp =
   TestLabel "non-mapped root path" $
   TestList [
     "path = []" ~:
     tc setup Nothing                  []

   , "path = [non-prefix]" ~:
     tc setup Nothing                  [nonPrefixPathComp]

   , "path = [non-prefix, empty]" ~:
      tc setup Nothing                 (nonPrefixPathComp : [""])

   , "path = [empty]" ~:
      tc setup Nothing                 [""]

   , "path = [empty, empty]" ~:
      tc setup Nothing                 ["", ""]
   ]

nonMappedNonRoot :: Sut.PathPrefixesSetup -> T.Text -> Test
nonMappedNonRoot setup nonMappedPathComp =
   TestLabel "non-mapped non-root path" $
   TestList [
     "path = [non-mapped]" ~:
     tc setup Nothing              [nonMappedPathComp]

   , "path = [non-mapped, other]" ~:
     tc setup Nothing              [nonMappedPathComp, "other comp"]

   , "path = [non-mapped, empty]" ~:
      tc setup Nothing             (nonMappedPathComp : [""])
   ]

check
  :: Sut.PathPrefixesSetup
  -> Sut.Result        -- ^ expected
  -> String            -- ^ test case description
  -> Sut.RequestPath   -- ^ actual path
  -> Test
check setup expected description actual =
  TestLabel description $ tc setup expected actual

tc
  :: Sut.PathPrefixesSetup
  -> Sut.Result        -- ^ expected
  -> Sut.RequestPath   -- ^ actual path
  -> Test
tc setup expected actual =
  TestCase $ assertEqual "" expected result
  where
    result = Sut.resolve setup actual
