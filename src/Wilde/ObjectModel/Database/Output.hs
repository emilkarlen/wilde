-- | Utilities related to database SQL generation.
module Wilde.ObjectModel.Database.Output
       (
         DATABASE_TABLE(..),
         COLUMN_NAMES(..),
         OUTPUT_FOR_EXISTING(..),
         OUTPUT_FOR_CREATE(..),
         IO_FOR_EXISTING(..),

         aOutputForCreate,

         aOutputExisting,
         aOutputsExisting,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Media.Database

import Wilde.ObjectModel.Database

import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils
import qualified Wilde.ObjectModel.Database as Database


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


aOutputForCreate :: OUTPUT_FOR_CREATE atConf
                 => Database.OutputerWithConnection
                    (AttributeForCreate atConf dbTable typeForExisting typeForCreate)
aOutputForCreate afc = atOutputerForCreate (attrfcType afc) (attrfcValue afc)

aOutputsExisting :: OUTPUT_FOR_EXISTING atConf
                 => [Any (Attribute atConf dbTable)]
                 -> ConvertResult [DatabaseOutput]
aOutputsExisting = mapM (OmUtils.anyValueApply aOutputExisting)

aOutputExisting :: OUTPUT_FOR_EXISTING atConf
                => DatabaseOutputer
                (Attribute atConf dbTable typeForExisting typeForCreate)
aOutputExisting a = atOutputerExisting (attrType a) (attrValue a)
