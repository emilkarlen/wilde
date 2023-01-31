module Wilde.Driver.Application.WaiServer.Cli.Help
(
    -- * CLI help
    header,
    footer,
    -- * configuration help
    printConfigurationHelp
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as M
import           Data.List (intersperse)
import qualified Data.Text as T

import qualified System.IO as IO

import           Options.Applicative.Help.Pretty

import qualified Data.Aeson.Key as AK

import qualified System.Console.Terminal.Size as TermSize

import qualified Wilde.Driver.Application.WaiServer.Cli.Options as Opts
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Read as ConfFileRead
import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Configuration as Conf

import qualified Wilde.Utils.Logging.Entry as Logging

import           Wilde.Driver.Application.WaiServer.Cli.Help.DocFormat as DF
import qualified Wilde.Driver.Application.WaiServer.Cli.Help.FormatConfFile as FormatConfFile
import qualified Data.Maybe


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


header :: String
          -- ^ Application name, to present in help.
          -- See help output for details.
       -> Doc
header appName = text "A web server implementing the" <+> text appName <+> text "web application."

footer :: Doc
footer = renderParas
    [
        PText $ "Use" <<>> long_opt Opts.print_conf_help_long <<>>
            "for help on configuration."
    ]

printConfigurationHelp :: IO ()
printConfigurationHelp = do
    outputWidth <- getOutputWidth
    displayIO IO.stdout $ simpleDoc outputWidth
    putStrLn ""
    where
        simpleDoc :: Int -> SimpleDoc
        simpleDoc consoleWidth = renderPretty 1 consoleWidth $ DF.render helpConfig

        getOutputWidth :: IO Int
        getOutputWidth = do
            Data.Maybe.fromMaybe 80 <$> getConsoleWidth

        getConsoleWidth :: IO (Maybe Int)
        getConsoleWidth = do
            mbSize <- TermSize.size
            pure $ TermSize.width <$> mbSize

logging_levels_ordered :: [Logging.Level]
logging_levels_ordered = enumFrom minBound

helpConfig :: DF.SectionContents
helpConfig = DF.SectionContents initParas [configSources, configFiles, loggingLevel]

configSources, configFiles, loggingLevel :: Section

initParas :: [Para]
initParas =
    [
        PText "Some parts of the server may be configured. \
              \This is useful especially for serving files."
    ]

configSources = parasSection "Configuration Sources"
    [
        PText "There are three sources of configuration:"
    ,   PList sourcesList
    ,   PText "Command line options overrides values from a configuration file, \
              \ which in turn overrides the builtin defaults."
    ,   PText $ "The" <<>> long_opt Opts.print_defaults_long <<>> "option prints \
                \the builtin defaults using the syntax of a configuration file."
    ]
    where
        sourcesList :: [String]
        sourcesList =
            [
                "builtin defaults"
            ,   "a configuration file" <<>> in_parens ("given by the" <<>> long_opt Opts.conf_file_long
                <<>> "option")
            ,   "command line options"
            ]

configFiles = Section "Configuration files" $ SectionContents initParas [port, servingFiles, logging]
    where
        initParas :: [Para]
        initParas =
            [
                PText $ "A configuration file is given via the"
                      <<>> long_opt Opts.conf_file_long
                      <<>> "option. It uses YAML syntax."

            ,   PText "A configuration file need not specify all parts of the configuration - \
                      \parts of the configuration that is not in the file \
                      \is derived from the other sources."
            ]

servingFiles, logging, port :: Section

servingFiles = Section "Serving Files" $ SectionContents initialParas [types]
    where
        initialParas :: [Para]
        initialParas =
            [
                PText $ "Serving of files is configured under" <<>> fmt_key ConfFileRead.field_files <> "."
            ,   PText "Two mappings can be configured:"
            ,   props
            ,   example
            ,   PText "A request path is served as a file if (and only if):"
            ,   file_serving_condition
            ]

        props :: Para
        props = PHdrValList $ HvlCompact
            [
                (AK.toString ConfFileRead.field_file_paths, se_rp_prefix <<>> "->" <<>> se_fsp_prefix)
            ,   (AK.toString ConfFileRead.field_mime_types, se_file_ext <<>> "->" <<>> se_mime_type)
            ]

        example :: Para
        example = PExample [FormatConfFile.asLiteral example_conf, example_explanation]

        file_serving_condition :: Para
        file_serving_condition = PHdrValList $ HvlParas
            [
                (
                    "it starts with a" <<>> se_rp_prefix <> ", and",
                    [as_in_example path_style ConfFileRead.field_file_paths]
                )

            ,   (
                    "its basename extension equals a" <<>> se_file_ext,
                    [as_in_example ext_style ConfFileRead.field_mime_types]
                )
            ]
            where
                as_in_example :: String -> AK.Key -> Para
                as_in_example value confFileKey = PText $ in_parens $
                    fmt_value_in_text value <<>> "in the example, as given under" <<>>
                    fmt_key confFileKey <> "."

        example_explanation :: Para
        example_explanation = PExamplification
            [
                ExText "The request path of"
            ,   ExLiteral $ fmt_path ["http://localhost", path_style, style_file]
            ,   ExText "is"
            ,   ExLiteral $ fmt_path [path_style, style_file]
            ,   ExText "which will be served as the file"
            ,   ExLiteral $ fmt_path ["~", fs_path_style,  style_file]
            ,   ExText "with the mime type"
            ,   ExLiteral  mt_style
            ]

        example_conf :: Conf.Configuration
        example_conf = Conf.Configuration Nothing (Just files) Nothing
            where
                files :: Conf.Files
                files = Conf.Files
                    {
                        Conf.file_paths = Just file_paths
                    ,   Conf.mime_types = Just mime_types
                    }
                file_paths :: Conf.FilePaths
                file_paths = M.fromList
                    [
                        ([T.pack path_style],fs_path_style)
                    ,   ([T.pack path_image], fs_path_image)
                    ]

        mime_types :: Conf.MimeTypeMapping
        mime_types = M.fromList
            [
                (T.pack ext_style,T.pack mt_style)
            ,   (T.pack ext_image,T.pack  mt_image)
            ]

        path_style, fs_path_style, path_image, fs_path_image :: String
        path_style = "style"
        fs_path_style = "my-styles/css"
        path_image = "image"
        fs_path_image = "/usr/share/my-images"

        ext_style, mt_style, ext_image, mt_image, style_file :: String
        ext_style = "css"
        mt_style = "text/css"
        ext_image = "png"
        mt_image = "image/png"

        style_file = "my." <> ext_style

        types :: Section
        types = parasSection "Types" [typesList]

        typesList :: Para
        typesList = PHdrValList $ HvlParas
            [
                (se_rp_prefix,
                [
                    PText "A sequence of \"whole\" path components (separated by \"/\")."
                ,   PExamplification
                    [
                        ExText    "Thus"
                    ,   ExLiteral "my/style"
                    ,   ExText    "is a prefix of"
                    ,   ExLiteral "my/style/css"
                    ,   ExText    "but not of"
                    ,   ExLiteral "my/style-css"
                    ]
                ]
                )
            ,   (se_fsp_prefix,
                [
                    PList
                    [
                        "A file system path, relative to the current directory."
                    ,   "Must be an existing directory in the local file system."
                    ]
                ]
                )
            ,   (se_file_ext,
                [
                    PText $ "The part of the basename of the request path that follows \
                            \the last" <<>> dot <<>> " (initial" <<>> dot <> "s are ignored)."
                ]
                )
            ,   (se_mime_type,
                [
                    PText "An IANA MIME media type."
                ]
                )
            ]
            where
                dot :: String
                dot = fmt_value_in_text "."

logging = parasSection "Logging"
    [
        PText $ "Logging is configured under" <<>> fmt_key ConfFileRead.field_logging <> "."
    ,   PText "Two (obvious) things can be configured:"
    ,   props
    ,   example
    ,   PText $ in_parens $ "Valid" <<>> se_level <<>> "values are listed below."
    ]
    where
        initParas :: [Para]
        initParas =
            [
                PText $ "Logging is configured under" <<>> fmt_key ConfFileRead.field_logging <> "."
            ,   PText "Two (obvious) things can be configured:"
            ,   props
            ,   PText $ in_parens $ "Valid" <<>> se_level <<>> "values are listed below."
            ,   example
            ]

        props :: Para
        props = PHdrValList $ HvlCompact
            [
                (AK.toString ConfFileRead.field_logging_enabled, "(false|true)")
            ,   (AK.toString ConfFileRead.field_logging_level, se_level)
            ]

        example :: Para
        example = PExample [FormatConfFile.asLiteral example_conf]

        example_conf :: Conf.Configuration
        example_conf = Conf.Configuration Nothing Nothing (Just logging)
            where
                logging :: Conf.Logging
                logging = Conf.Logging
                    {
                        Conf.enabled    = Just True
                    ,   Conf.level      = Just Logging.DEBUG
                    }


port = parasSection "Port"
    [
        PText "Configuration of which port the server listens on."
    ,   example
    ]
    where
        initParas :: [Para]
        initParas =
            [
                PText "Configuration of which port the server listens on."
            ,   example
            ]

        example :: Para
        example = PExample [FormatConfFile.asLiteral example_conf]

        example_conf :: Conf.Configuration
        example_conf = Conf.Configuration (Just 8080) Nothing Nothing

loggingLevel = Section se_level $ SectionContents initialParas [usage]
    where
        initialParas :: [Para]
        initialParas =
            [
                PText $ "Valid values for " <<>> se_level <> ":"
            ,   PList $ map show logging_levels_ordered
            ,   PText $ "Each level \"includes\" all levels below it." <<>>
                        "Thus, setting the level to" <<>> show Logging.INFO <<>>
                        "will show log output for" <<>>
                        and_list (map show $ enumFrom Logging.INFO) <> "."
            ,   PText $ "The" <<>> show Logging.LIBRARY <<>>
            "level provides some hard coded logging in the Wilde library."
            ]

        usage :: Section
        usage = parasSection "Usage"
            [
                PText "The logging level can be configured via"
            ,   PList
                [
                    "Command line argument" <<>> long_opt Opts.logging_level_long
                ,   fmt_key ConfFileRead.field_logging <<>> "section of configuration files"
                ]
            ]

in_parens :: String -> String
in_parens s = '(' : s <> ")"

(<<>>) :: String -> String -> String
a <<>> b = a <> " " <> b

se_level, se_rp_prefix, se_fsp_prefix, se_file_ext, se_mime_type :: String
se_level = "LOGGING-LEVEL"
se_rp_prefix = "REQUEST-PATH-PREFIX"
se_fsp_prefix = "FILE-SYSTEM-PATH-PREFIX"
se_file_ext = "FILE-EXTENSION"
se_mime_type = "MIME-TYPE"

and_list :: [String] -> String
and_list []     = ""
and_list [x]    = x
and_list [x,y]  = x <<>> "and" <<>> y
and_list (x:xs) = x <> "," <<>> and_list xs


long_opt :: String -> String
long_opt o = "'--" <> o <> "'"

fmt_key :: AK.Key -> String
fmt_key = show

fmt_value_in_text :: String -> String
fmt_value_in_text = show

fmt_path_d :: [String] -> Doc
fmt_path_d = text . fmt_path

fmt_path :: [String] -> String
fmt_path [] = error "fmt_path: must contain at least one component"
fmt_path xs = concat $ intersperse "/" xs

key_logging_level :: [String]
key_logging_level = [AK.toString ConfFileRead.field_logging, AK.toString ConfFileRead.field_logging_level]
