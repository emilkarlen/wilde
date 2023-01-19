-- | Option names.
module Wilde.Driver.Application.WaiServer.Cli.Options
    (
        -- * Options

        port_long,
        port_short,

        conf_file_long,
        conf_file_short,

        logging_enabled_long,
        logging_level_long,

        print_defaults_long,
        print_conf_help_long,

        -- Utils for booleans

        values_true,
        values_false,
        value_true_std,
        value_false_std,

        read_bool,
    )
where

import Data.List


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


port_long, conf_file_long, print_defaults_long, print_conf_help_long,
    logging_enabled_long, logging_level_long :: String

port_short, conf_file_short :: Char

port_long = "port"
port_short = 'p'

conf_file_long = "config-file"
conf_file_short = 'f'

print_defaults_long = "dump-defaults"
print_conf_help_long = "help-conf"

logging_enabled_long = "log"
logging_level_long = "log-level"

values_true, values_false :: [String]
value_true_std, value_false_std :: String

value_true_std  = "on"
value_false_std = "off"

values_true  = value_true_std  : ["true" , "1"]
values_false = value_false_std : ["false", "0"]

read_bool :: String -> Either String Bool
read_bool s | s `elem` values_false = Right False
            | s `elem` values_true  = Right True
            | otherwise             = Left $ read_bool_err_msg s

read_bool_err_msg :: String -> String
read_bool_err_msg s = "Not a boolean: `" <> s <> "'. Expecting one of " <> valid_values_str <> "."
    where
        valid_values :: [String]
        valid_values = values_false <> values_true
        valid_values_str :: String
        valid_values_str = intercalate ", " valid_values
