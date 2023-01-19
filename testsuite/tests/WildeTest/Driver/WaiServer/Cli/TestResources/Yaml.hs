module WildeTest.Driver.WaiServer.Cli.TestResources.Yaml
(
    yaml_file,
    yaml_map_file,

    file_paths_contents,
    mime_types_contents,
    logging_contents,
    files_contents,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.String as S
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as M

import           System.Exit (ExitCode)

import qualified Options.Applicative as OptParse
import qualified Data.Yaml as Yaml
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import           Data.Scientific (scientific)

import           Test.HUnit.Base
import           Test.HUnit.Lang (FailureReason(..), formatFailureReason)

import           Wilde.Driver.Application.WaiServer.Cli.Arguments
import qualified Wilde.Driver.Application.WaiServer.Cli.Parse as Parse
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Read as CFP
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Configuration as Configuration
import Wilde.Driver.Application.WaiServer.RequestHandling.Types (FilePaths)
import Wilde.Driver.Application.WaiServer.Application (MimeTypeMapping)


import           TestResources.Testing.Checker

import           TestResources.Testing.AssertUtils
import qualified TestResources.Testing.TmpDir as TmpDir


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


file_paths_contents :: [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)]
file_paths_contents [] = []
file_paths_contents file_paths = [(CFP.field_files, Yaml.Object $ AKM.fromList file_paths')]
    where
        file_paths' :: [(AK.Key, Yaml.Value)]
        file_paths' = [(CFP.field_file_paths, Yaml.Object $ AKM.fromList file_paths)]

mime_types_contents :: [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)]
mime_types_contents [] = []
mime_types_contents mime_types = [(CFP.field_files, Yaml.Object $ AKM.fromList mime_types')]
    where
        mime_types' :: [(AK.Key, Yaml.Value)]
        mime_types' = [(CFP.field_mime_types, Yaml.Object $ AKM.fromList mime_types)]

files_contents :: [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)]
files_contents file_paths mime_types = [(CFP.field_files, file_elements)]
    where
        fp_object :: Yaml.Value
        fp_object = Yaml.Object $ AKM.fromList file_paths

        mt_object :: Yaml.Value
        mt_object = Yaml.Object $ AKM.fromList mime_types

        file_elements :: Yaml.Value
        file_elements = Yaml.Object $ AKM.fromList
            [(CFP.field_file_paths, fp_object), (CFP.field_mime_types, mt_object)]

logging_contents :: [(AK.Key, Yaml.Value)]-> [(AK.Key, Yaml.Value)]
logging_contents logging_entries = [(CFP.field_logging, elements)]
    where
        elements :: Yaml.Value
        elements = Yaml.Object $ AKM.fromList logging_entries


yaml_file :: String -> Yaml.Value -> TmpDir.DirPopulator
yaml_file fileName contents = TmpDir.file_b fileName $ Yaml.encode contents

yaml_map_file :: String -> [(AK.Key, Yaml.Value)] -> TmpDir.DirPopulator
yaml_map_file fileName contents = yaml_file fileName $ Yaml.Object $ AKM.fromList contents
