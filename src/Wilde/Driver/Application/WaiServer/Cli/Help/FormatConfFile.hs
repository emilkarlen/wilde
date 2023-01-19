module Wilde.Driver.Application.WaiServer.Cli.Help.FormatConfFile
(
    asLiteral,
)
 where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Yaml as Yaml

import qualified Wilde.Driver.Application.WaiServer.ConfigFile.Configuration as Conf
import           Wilde.Driver.Application.WaiServer.ConfigFile.Yaml ()
import qualified Wilde.Driver.Application.WaiServer.Cli.Help.DocFormat as DF


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


asLiteral :: Conf.Configuration -> DF.Para
asLiteral conf = DF.PLiteral as_str
    where
        as_bs :: B.ByteString
        as_bs = Yaml.encode $ Yaml.toJSON conf

        as_str :: String
        as_str = T.unpack $ TEnc.decodeUtf8 as_bs
