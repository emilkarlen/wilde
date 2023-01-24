-- | Configuration of an appliation that is independent of the
-- Driver.
module Wilde.Application.ApplicationConfiguration
       (
          module Wilde.Application.ApplicationServices,

          ServiceLinks(..),
          ApplicationConfiguration(..),

          Translations.Translations(..),

          DbConf.Configuration(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.Logging.Class as Logger

import qualified Wilde.Media.Database.Configuration as DbConf
import qualified Wilde.Media.Translations as Translations
import           Wilde.Media.Presentation (ServiceLinks(..))

import           Wilde.Application.ApplicationServices


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data ApplicationConfiguration =
     ApplicationConfiguration
     {
       appServices                  :: ApplicationServices
     , translations                 :: Translations.Translations
     , dbConfiguration              :: DbConf.Configuration
     , serviceLinks                 :: ServiceLinks
     , appCssFile                   :: Maybe String
     , appLogger                    :: Logger.AnyLogger
     }
