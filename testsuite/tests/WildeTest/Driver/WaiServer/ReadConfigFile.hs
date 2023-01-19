{-# LANGUAGE OverloadedStrings #-}
module WildeTest.Driver.WaiServer.ReadConfigFile
      (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Map as M

import qualified Data.Yaml as Yaml
import qualified TestResources.Testing.TmpDir as TmpDir
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK

import qualified Data.Text.Encoding as TextEncoding

import           Test.HUnit hiding (failures)
import qualified TestResources.Testing.Checker as C
import qualified TestResources.Testing.TestWithTmpDir as TestWithTmpDir
import           WildeTest.Driver.WaiServer.Cli.TestResources.Yaml

import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Read as Sut
import Wilde.Driver.Application.WaiServer.RequestHandling.Types (FilePaths)
import Wilde.Driver.Application.WaiServer.RequestHandling.File.Types (MimeTypeMapping)
import Wilde.Utils.Logging.Entry (Level(..))
import Wilde.Driver.Application.WaiServer.Cli.Read (Logging(..))


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  TestLabel "parseConfigFile" $
  TestList
  [
    "generic successes" ~: generic_successes

  , "generic failures"  ~: generic_failures

  , "individual field"  ~: individual_fields

  , "all fields"        ~: all_fields
  ]

generic_successes :: Test
generic_successes =
  TestList
  [
    "empty" ~:
    TestCase $ TestWithTmpDir.assertWithTmpDir (TmpDir.empty_file "conf.yaml") $
    assertReadFile "conf.yaml" (isSuccessfulReadOf emptyConfig)
  ]

generic_failures :: Test
generic_failures =
  TestList
  [
    "file" ~:
    TestList
    [
      "file is a dir" ~:
      TestCase $ TestWithTmpDir.assertWithTmpDir (TmpDir.empty_dir "a-dir") $
      assertReadFile "a-dir" isFailure

    , "file does not exist" ~:
      TestCase $ TestWithTmpDir.assertWithTmpDir TmpDir.empty $
      assertReadFile "non-existing.yaml" isFailure
    ]

  , "unknown field" ~:
    let
      contents :: [(AK.Key, Yaml.Value)]
      contents = [(Sut.field_port, Yaml.Number 1),
                  (AK.fromString "unknown", Yaml.String "value")]
    in
      TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "unknown.yaml" contents) $
      assertReadFile "unknown.yaml" isFailure
  ]

individual_fields :: Test
individual_fields =
  TestList
  [
    "port"       ~: port
  , "file-paths" ~: file_paths
  , "mime-types" ~: mime_types
  , "logging"    ~: logging
  ]

port :: Test
port =
  TestList
  [
    "valid" ~:
    let
      expected :: Num a => a
      expected = 5
      contents = [(Sut.field_port, Yaml.Number expected)]
    in
      TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "port.yaml" contents) $
      assertReadFile "port.yaml" (isSuccessfulReadOf (emptyConfig { Sut.port = Just expected}))

  , "invalid - string" ~:
    let
      contents = [(Sut.field_port, Yaml.String "5")]
    in
      TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "port.yaml" contents) $
      assertReadFile "port.yaml" isFailure
  ]

file_paths :: Test
file_paths =
  TestList
  [
    "valid" ~:
    TestList
    [
      "single mapping" ~:
      TestList
      [
        "request path: single component" ~:
        let
          request_path, fs_dir :: S.IsString a => a
          request_path = "style"
          fs_dir       = "style-dir"
          expected :: FilePaths
          expected = M.singleton [request_path] fs_dir
          contents :: [(AK.Key, Yaml.Value)]
          contents = [(AK.fromString request_path, Yaml.String fs_dir)]
        in
          TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "file-paths.yaml" (file_paths_contents contents)) $
          assertReadFile "file-paths.yaml"
          (isSuccessfulReadOf (emptyConfig { Sut.files = Just emptyFiles { Sut.file_paths = Just expected}}))

      , "request path: multiple components" ~:
        let
          request_path1, request_path2, config_file_key, fs_dir :: (Semigroup a, S.IsString a) => a
          request_path1   = "super"
          request_path2   = "sub"
          config_file_key = request_path1 <> "/" <> request_path2
          fs_dir          = "corresponding-dir"
          expected :: FilePaths
          expected = M.singleton [request_path1, request_path2] fs_dir
          contents :: [(AK.Key, Yaml.Value)]
          contents = [(AK.fromString config_file_key, Yaml.String fs_dir)]
        in
          TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "file-paths.yaml" (file_paths_contents contents)) $
          assertReadFile "file-paths.yaml"
          (isSuccessfulReadOf (emptyConfig { Sut.files = Just emptyFiles { Sut.file_paths = Just expected}}))
      ]
    , "multiple mappings" ~:
      let
        request_path1, request_path2, fs_dir1, fs_dir2 :: S.IsString a => a
        request_path1   = "path_1"
        request_path2   = "path_2"
        fs_dir1         = "fs_path_1"
        fs_dir2         = "fs_path_2"
        expected :: FilePaths
        expected = M.fromList [([request_path1], fs_dir1),
                               ([request_path2], fs_dir2)]
        contents :: [(AK.Key, Yaml.Value)]
        contents = [(AK.fromString request_path1, Yaml.String fs_dir1),
                    (AK.fromString request_path2, Yaml.String fs_dir2)]
      in
        TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "file-paths.yaml" (file_paths_contents contents)) $
        assertReadFile "file-paths.yaml"
        (isSuccessfulReadOf (emptyConfig { Sut.files = Just emptyFiles { Sut.file_paths = Just expected}}))
    ]
  , "invalid" ~:
    TestList
    [
      "request path starts with /" ~:
      let
        contents = [(AK.fromString "/path", Yaml.String "valid_dir")]
      in
        TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "file-paths.yaml" (file_paths_contents contents)) $
        assertReadFile "file-paths.yaml" isFailure

    , "invalid file system path type" ~:
      let
        contents = [(AK.fromString "valid", Yaml.Array mempty)]
      in
        TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "file-paths.yaml" (file_paths_contents contents)) $
        assertReadFile "file-paths.yaml" isFailure
    ]
  ]

mime_types :: Test
mime_types =
  TestList
  [
    "valid" ~:
    TestList
    [
      "single mapping" ~:
      let
        file_extension, mime_type :: S.IsString a => a
        file_extension = "ext"
        mime_type       = "the/mime/type"
        expected :: MimeTypeMapping
        expected = M.singleton file_extension mime_type
        contents :: [(AK.Key, Yaml.Value)]
        contents = [(AK.fromString file_extension, Yaml.String mime_type)]

        expected_config :: Sut.Configuration
        expected_config = emptyConfig { Sut.files = Just emptyFiles { Sut.mime_types = Just expected}}
      in
        TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "mime-types.yaml" (mime_types_contents contents)) $
        assertReadFile "mime-types.yaml"
        (isSuccessfulReadOf expected_config)
    , "multiple mappings" ~:
      let
        extension1, extension2, mime_type1, mime_type2 :: S.IsString a => a
        extension1 = "ext-1"
        extension2 = "ext-2"
        mime_type1 = "the/mime/type/1"
        mime_type2 = "the/mime/type/2"
        expected :: MimeTypeMapping
        expected = M.fromList [(extension1, mime_type1),
                               (extension2, mime_type2)]
        contents :: [(AK.Key, Yaml.Value)]
        contents = [(AK.fromString extension1, Yaml.String mime_type1),
                    (AK.fromString extension2, Yaml.String mime_type2)]
      in
        TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "mime-types.yaml" (mime_types_contents contents)) $
        assertReadFile "mime-types.yaml" (isSuccessfulReadOf
        (emptyConfig { Sut.files = Just emptyFiles { Sut.mime_types = Just expected}}))
    ]
  , "invalid mime type JSON type" ~:
    let
      contents = [(AK.fromString "valid", Yaml.Array mempty)]
    in
      TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "mime-types.yaml" (mime_types_contents contents)) $
      assertReadFile "mime-types.yaml" isFailure
  ]

logging :: Test
logging =
  TestList
  [
    "successful" ~: logging_successful
  , "failure"    ~: logging_failure
  ]

logging_successful :: Test
logging_successful =
  TestList
  [
    "no fields given given" ~:
    let
      contents :: [(AK.Key, Yaml.Value)]
      contents = []

      expected :: Sut.Configuration
      expected = emptyConfig { Sut.logging = Just emptyLogging }
    in
      is_success contents expected

  ,   "only enabled given" ~:
      TestList
      [
        show expected_enabled ~: only_enabled expected_enabled
      | expected_enabled <- enumFrom minBound
      ]

  ,  "only level given" ~:
      TestList
      [
        show expected_level ~: only_level expected_level
      | expected_level <- enumFrom minBound
      ]
  ,   "all entries given" ~:
    let
      enabled_expected :: Bool
      enabled_expected = True

      level_expected :: Level
      level_expected = DEBUG

      contents :: [(AK.Key, Yaml.Value)]
      contents =
        [
          (Sut.field_logging_enabled, Yaml.toJSON enabled_expected)
        , (Sut.field_logging_level,   Yaml.toJSON (show level_expected))
        ]
      expected :: Sut.Configuration
      expected = emptyConfig {
        Sut.logging = Just $
          Sut.Logging
            {
              Sut.enabled = Just enabled_expected
            , Sut.level   = Just level_expected
            }
        }
    in
      is_success contents expected
  ]
  where
    is_success :: [(AK.Key, Yaml.Value)] -> Sut.Configuration -> Test
    is_success logging_fields expected =
      TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "logging.yaml" (logging_contents logging_fields)) $
      assertReadFile "logging.yaml"
      (isSuccessfulReadOf expected)

    only_level :: Level -> Test
    only_level expected_level =
      let
        contents :: [(AK.Key, Yaml.Value)]
        contents =
          [
            (Sut.field_logging_level, Yaml.toJSON (show expected_level))
          ]
        expected :: Sut.Configuration
        expected = emptyConfig { Sut.logging = Just emptyLogging { Sut.level = Just expected_level } }
      in
        is_success contents expected

    only_enabled :: Bool -> Test
    only_enabled expected_enabled =
      let
        contents :: [(AK.Key, Yaml.Value)]
        contents =
          [
            (Sut.field_logging_enabled, Yaml.toJSON expected_enabled)
          ]

        expected :: Sut.Configuration
        expected = emptyConfig { Sut.logging = Just emptyLogging { Sut.enabled = Just expected_enabled } }
      in
        is_success contents expected

logging_failure :: Test
logging_failure =
  TestList
  [
    "invalid value of valid field" ~: invalid_value
  , "invalid field" ~:
    is_failure [("invalid_key", Yaml.String "the_value")]
  , "invalid type of value" ~:
    is_failure_top_level [(Sut.field_logging, emptyList)]
  ]
  where
    is_failure ::  [(AK.Key, Yaml.Value)] -> Test
    is_failure contents = is_failure_top_level (logging_contents contents)

    is_failure_top_level ::  [(AK.Key, Yaml.Value)] -> Test
    is_failure_top_level top_level_contents =
      TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "logging.yaml" top_level_contents) $
      assertReadFile "logging.yaml"
      isFailure

    invalid_value :: Test
    invalid_value = TestList
      [
        "enabled" ~:
        TestList
        [
          "invalid value" ~: is_failure [(Sut.field_logging_enabled, Yaml.String "invalid_bool")]
        , "invalid type"  ~: is_failure [(Sut.field_logging_enabled, emptyList)]
        ]
      , "level"   ~:
        TestList
        [
          "invalid value" ~: is_failure [(Sut.field_logging_level, Yaml.String "invalid_level")]
        , "invalid type"  ~: is_failure [(Sut.field_logging_level, emptyList)]
        ]
      ]

all_fields :: Test
all_fields =
  TestCase $ TestWithTmpDir.assertWithTmpDir (yaml_map_file "all-fields.yaml" contents) $
  assertReadFile "all-fields.yaml" (isSuccessfulReadOf expected)

  where
    port_expected :: Num a => a
    port_expected = 5
    port_contents :: [(AK.Key, Yaml.Value)]
    port_contents = [(Sut.field_port, Yaml.Number port_expected)]

    fp_request_path, fp_fs_dir :: S.IsString a => a
    fp_request_path = "style"
    fp_fs_dir       = "style-dir"
    fp_expected :: FilePaths
    fp_expected = M.singleton [fp_request_path] fp_fs_dir
    fp_contents :: [(AK.Key, Yaml.Value)]
    fp_contents = [(AK.fromString fp_request_path, Yaml.String fp_fs_dir)]

    mt_file_extension, mt_mime_type :: S.IsString a => a
    mt_file_extension = "ext"
    mt_mime_type      = "the/mime/type"
    mt_expected :: MimeTypeMapping
    mt_expected = M.singleton mt_file_extension mt_mime_type
    mt_contents :: [(AK.Key, Yaml.Value)]
    mt_contents = [(AK.fromString mt_file_extension, Yaml.String mt_mime_type)]

    fs_contents :: [(AK.Key, Yaml.Value)]
    fs_contents = files_contents fp_contents mt_contents
    lg_enabled_expected :: Bool
    lg_enabled_expected = True
    lg_level_expected :: Level
    lg_level_expected = DEBUG
    lg_contents :: [(AK.Key, Yaml.Value)]
    lg_contents = logging_contents
      [
        (Sut.field_logging_enabled, Yaml.toJSON lg_enabled_expected)
      , (Sut.field_logging_level,   Yaml.toJSON (show lg_level_expected))
      ]

    contents :: [(AK.Key, Yaml.Value)]
    contents = port_contents <> fs_contents <> lg_contents

    expected :: Sut.Configuration
    expected = Sut.Configuration
      {
          Sut.port  = Just port_expected
      ,   Sut.files = Just Sut.Files
            {
                Sut.file_paths = Just fp_expected
            ,   Sut.mime_types = Just mt_expected
            }
      ,   Sut.logging = Just Sut.Logging
            {
                Sut.enabled    = Just lg_enabled_expected
            ,   Sut.level      = Just lg_level_expected
            }
      }



-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


type ReadConfigChecker = C.Checker (Either String Sut.Configuration)

assertReadFile :: String            -- ^ file name, relative tmp dir
               -> ReadConfigChecker -- ^ expectation
               -> FilePath          -- ^ file to read
               -> Assertion
assertReadFile relativeFileName checker tmpDir = do
  let configFilePath = tmpDir <> "/" <> relativeFileName
  actual <- Sut.read configFilePath
  C.assert checker actual

emptyConfig :: Sut.Configuration
emptyConfig = Sut.Configuration Nothing Nothing Nothing

emptyFiles :: Sut.Files
emptyFiles = Sut.Files Nothing Nothing

emptyLogging :: Sut.Logging
emptyLogging = Sut.Logging Nothing Nothing

emptyList :: Yaml.Value
emptyList = Yaml.toJSON ([] :: [Int])

isSuccessfulReadOf :: Sut.Configuration -> ReadConfigChecker
isSuccessfulReadOf expected = C.isRightOf $ C.eq expected

isFailure :: ReadConfigChecker
isFailure = C.isLeftOf C.anything
