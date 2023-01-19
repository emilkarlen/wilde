-- | Implementation of ToJASON of `Configuration`
{-# LANGUAGE OverloadedStrings #-}
module Wilde.Driver.Application.WaiServer.ConfigFile.Yaml
(
    file_paths_contents,
    mime_types_contents,
)
 where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as M

import qualified Data.Yaml as Yaml
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK

import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Read as CFP
import           Wilde.Driver.Application.WaiServer.ConfigFile.Configuration


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


instance Yaml.ToJSON Configuration where
    toJSON (Configuration mbPort mbFiles mbLogging) =
        case entries of
            [] -> Yaml.Null
            xs -> Yaml.Object $ AKM.fromList xs
        where
            translations = [ toMapEntries mbPort trPort
                           , toMapEntries mbFiles trFiles
                           , toMapEntries mbLogging trLogging
                           ]
            entries = concat translations

            trPort :: Int -> [(AK.Key, Yaml.Value)]
            trPort n = [(CFP.field_port, Yaml.toJSON n)]

            trFiles :: Files -> [(AK.Key, Yaml.Value)]
            trFiles (Files mbPaths mbMimeTypes) = files_contents fps mts
                where
                    fps, mts :: [(AK.Key, Yaml.Value)]
                    fps = toMapEntries mbPaths     trFps
                    mts = toMapEntries mbMimeTypes trMtm

            trFps :: FilePaths -> [(AK.Key, Yaml.Value)]
            trFps fps =
                [(AK.fromText (flatten_rqp req_path), Yaml.toJSON file_path)
                 | (req_path,file_path) <- M.toList fps
                ]
                where
                    flatten_rqp :: [T.Text] -> T.Text
                    flatten_rqp xs = T.concat $ L.intersperse "/" xs

            trMtm :: MimeTypeMapping -> [(AK.Key, Yaml.Value)]
            trMtm mtm =
                [
                    (AK.fromText ext, Yaml.toJSON mime_type)
                    | (ext, mime_type) <- M.toList mtm
                ]

            trLogging :: Logging -> [(AK.Key, Yaml.Value)]
            trLogging (Logging mbEnabled mbLevel) =
                [(CFP.field_logging, Yaml.Object $ AKM.fromList fields) | not (null fields)]
                where
                    fields_enabled :: [(AK.Key, Yaml.Value)]
                    fields_enabled = maybe [] (\b -> [(CFP.field_logging_enabled, Yaml.toJSON b)]) mbEnabled

                    fields_level :: [(AK.Key, Yaml.Value)]
                    fields_level = maybe [] (\lvl -> [(CFP.field_logging_level, Yaml.toJSON (show lvl))]) mbLevel

                    fields :: [(AK.Key, Yaml.Value)]
                    fields = fields_enabled <> fields_level


            toMapEntries :: Maybe a -> (a -> [(AK.Key, Yaml.Value)]) -> [(AK.Key, Yaml.Value)]
            toMapEntries mbX x2l = maybe [] x2l mbX


file_paths_contents :: [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)]
file_paths_contents [] = []
file_paths_contents file_paths = [(CFP.field_file_paths, Yaml.Object $ AKM.fromList file_paths)]

mime_types_contents :: [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)]
mime_types_contents [] = []
mime_types_contents mime_types = [(CFP.field_mime_types, Yaml.Object $ AKM.fromList mime_types)]

files_contents :: [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)] -> [(AK.Key, Yaml.Value)]
files_contents paths mime_types =
    [(CFP.field_files, Yaml.Object $ AKM.fromList entries) | not (null entries)]
    where
        entries :: [(AK.Key, Yaml.Value)]
        entries = file_paths_contents paths <> mime_types_contents mime_types
