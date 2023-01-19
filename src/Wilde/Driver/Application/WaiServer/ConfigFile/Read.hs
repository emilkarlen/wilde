{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.ConfigFile.Read
(
    ConfFile.Configuration(..),
    ConfFile.Files(..),
    ConfFile.Logging(..),

    read,

    -- * file keys

    field_port,
    field_files,
    field_file_paths,
    field_mime_types,
    field_logging,
    field_logging_enabled,
    field_logging_level,
)
 where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Prelude hiding (read)
import qualified Data.List as L
import qualified Data.String as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Read as R


import           Data.Yaml
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.KeyMap as AKM

import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Configuration as ConfFile -- (Configuration(..))
import           Wilde.Driver.Application.WaiServer.RequestHandling.Types (FilePaths)
import           Wilde.Driver.Application.WaiServer.RequestHandling.File.Types (MimeTypeMapping)
import Wilde.Utils.Logging.Entry (Level)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


root_all_fields, files_all_fields, all_fields_logging :: [Aeson.Key]
root_all_fields = [field_port, field_files, field_logging]
files_all_fields = [field_file_paths, field_mime_types]
all_fields_logging = [field_logging_enabled, field_logging_level]

field_port, field_files, field_file_paths, field_mime_types :: S.IsString a => a -- Aeson.Key
field_port = "port"
field_files = "files"
field_file_paths = "paths"
field_mime_types = "mime-types"
field_logging = "logging"
field_logging_enabled = "enabled"
field_logging_level = "level"

config_empty :: ParsableConfiguration
config_empty = ParsableConfiguration Nothing Nothing Nothing

files_empty :: ParsableFiles
files_empty = ParsableFiles Nothing Nothing

logging_empty :: ParsableLogging
logging_empty = ParsableLogging Nothing Nothing

-- | Helper type for YAML parsing
newtype ParsableFilePaths = ParsableFilePaths (M.Map T.Text T.Text)

-- | Helper type for YAML parsing
newtype ParsableMimeTypes = ParsableMimeTypes (M.Map T.Text T.Text)

plainMimeTypes :: ParsableMimeTypes -> MimeTypeMapping
plainMimeTypes (ParsableMimeTypes text_2_text) = T.unpack <$> text_2_text

-- | Corresponds to `Configuration`,
-- used for Yaml parsing via instances of `FromJSON`.
data ParsableConfiguration =
    ParsableConfiguration
    {
        port'       :: Maybe Int
    ,   files'      :: Maybe ParsableFiles
    ,   logging'    :: Maybe ParsableLogging
    }

data ParsableFiles =
    ParsableFiles
    {
        file_paths' :: Maybe ParsableFilePaths
    ,   mime_types' :: Maybe ParsableMimeTypes
    }

data ParsableLogging =
    ParsableLogging
    {
        logging_enabled' :: Maybe Bool
    ,   logging_level'   :: Maybe Level
    }

toConfiguration :: ParsableConfiguration -> Either String ConfFile.Configuration
toConfiguration
    ParsableConfiguration
    {
        port'       = thePort
    ,   files'      = theFiles
    ,   logging'    = theLogging
    } =
    do
        files <- toFiles theFiles
        pure $ ConfFile.Configuration thePort files (logging <$> theLogging)
    where
        toFiles :: Maybe ParsableFiles -> Either String (Maybe ConfFile.Files)
        toFiles Nothing = pure Nothing
        toFiles
            (Just (ParsableFiles
            {
                file_paths' = theFilePaths
            ,   mime_types' = theMimeTypes
            })) = do
                fp <- extractFilePaths theFilePaths
                let mt = plainMimeTypes <$> theMimeTypes
                let no_file_paths = maybe True M.null fp
                let no_mime_types = maybe True M.null mt
                if no_file_paths && no_mime_types
                    then pure Nothing
                    else pure $ Just $ ConfFile.Files fp mt

        logging :: ParsableLogging -> ConfFile.Logging
        logging (ParsableLogging mbEnabled mbLevel) =
            ConfFile.Logging mbEnabled mbLevel

extractFilePaths :: Maybe ParsableFilePaths -> Either String (Maybe FilePaths)
extractFilePaths Nothing = Right Nothing
extractFilePaths (Just (ParsableFilePaths text_to_text)) =
    let
        keyIsInvalid :: T.Text -> Bool
        keyIsInvalid req_path = req_path == "" || "" /= T.takeWhile (=='/') req_path

        translateKey :: T.Text -> [T.Text]
        translateKey = T.splitOn "/"

        translateFsPath :: T.Text -> String
        translateFsPath = T.unpack

        invalid_req_paths = filter keyIsInvalid $ M.keys text_to_text
    in
        if null invalid_req_paths
            then Right $ Just $ M.map translateFsPath $ M.mapKeys translateKey text_to_text
            else Left $ "Invalid request paths: " <> show invalid_req_paths

read :: FilePath -> IO (Either String ConfFile.Configuration)
read configFilePath = do
    result <- decodeConfFileEither configFilePath
    pure $ either (Left . prettyPrintParseException) toConfiguration result

instance FromJSON ParsableConfiguration where
    parseJSON v = parseJsonObject root_all_fields config_empty parser v
        where
            parser :: AKM.KeyMap Value -> Aeson.Parser ParsableConfiguration
            parser v =
                ParsableConfiguration
                <$> v .:? field_port
                <*> v .:? field_files
                <*> v .:? field_logging

instance FromJSON ParsableFiles where
    parseJSON v = parseJsonObject files_all_fields files_empty parser v
        where
            parser :: AKM.KeyMap Value -> Aeson.Parser ParsableFiles
            parser v =
                ParsableFiles
                <$> v .:? field_file_paths
                <*> v .:? field_mime_types

parseJsonObject :: [Aeson.Key] -> a -> (AKM.KeyMap Value -> Parser a) -> Value -> Parser a
parseJsonObject all_fields empty parser (Object v) =
    if null invalid_keys
        then parser v
        else fail $ "Invalid keys: " <> show invalid_keys
    where
        invalid_keys :: [Aeson.Key]
        invalid_keys = AKM.keys v L.\\ all_fields

parseJsonObject _ empty _ Null = pure empty
parseJsonObject _ _ _ invalid =
    Aeson.prependFailure "Syntax error, "
        (Aeson.typeMismatch "Object" invalid)

instance FromJSON ParsableFilePaths where
    parseJSON o@(Object _) = ParsableFilePaths <$> parseJSON o

    parseJSON invalid =
        Aeson.prependFailure "Syntax error, "
            (Aeson.typeMismatch "Object" invalid)

instance FromJSON ParsableMimeTypes where
    parseJSON o@(Object _) = ParsableMimeTypes <$> parseJSON o

    parseJSON invalid =
        Aeson.prependFailure ("Syntax error (" <> field_mime_types <> "), ")
            (Aeson.typeMismatch "Object" invalid)

instance FromJSON ParsableLogging where
    parseJSON v = parseJsonObject all_fields_logging logging_empty parser v
        where
            parser :: AKM.KeyMap Value -> Aeson.Parser ParsableLogging
            parser v =
                ParsableLogging
                <$> v .:? field_logging_enabled
                <*> v .:? field_logging_level

instance FromJSON Level where
    parseJSON = A.withText "Level" (parseEnumFromString . T.unpack)
        where
            parseEnumFromString :: String -> Parser Level
            parseEnumFromString s =
                let
                    mbLevel :: Maybe Level
                    mbLevel = R.readMaybe s
                in
                    maybe (fail $ "Invalid logging level: " <> s) pure mbLevel

decodeConfFileEither :: FilePath -> IO (Either ParseException ParsableConfiguration)
decodeConfFileEither = Data.Yaml.decodeFileEither
