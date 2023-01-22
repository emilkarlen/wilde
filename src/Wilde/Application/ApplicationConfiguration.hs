-- | Configuration of an appliation that is independent of the
-- Driver.
module Wilde.Application.ApplicationConfiguration
       (
         module Wilde.Application.ApplicationServices,

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

import Wilde.Application.ApplicationServices
import Wilde.Application.StandardServices
import qualified Wilde.Media.Presentation as Presentation
import Wilde.Application.StandardServiceLinks


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Configuration of an application
-------------------------------------------------------------------------------


data ApplicationConfiguration =
     ApplicationConfiguration
     {
       appServices                  :: ApplicationServices
     , translations                 :: Translations.Translations
     , dbConfiguration              :: DbConf.Configuration
     , standardServiceLinkRenderer  :: StandardServiceLinkRenderer
     , getMkStdObjectTypeService    :: Presentation.Monad MkObjectTypeServiceLink
     , getMkStdObjectService        :: Presentation.Monad MkObjectServiceLink
     , getMkGenericServiceLink      :: Presentation.Monad Presentation.MkGenericServiceLink
     , appCssFile                   :: Maybe String
     , appLogger                    :: Logger.AnyLogger
     }
