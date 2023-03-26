-- | Import qualified.

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectListFooters
(
  none,
  numberOfObjects,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.Accumulator as Acc

import           Wilde.WildeUi.TableUtils (dataCellSpaned)
import           Wilde.WildeUi.StdValueTypes (IntValue(..))
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS
import qualified Wilde.ObjectModel.Presentation.FooterRowsConstructor as F


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A 'GetMkFooterRowsConstructor' that generates no footer rows.
none :: OLS.GetMkFooterRowsConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
none = pure F.mkNoFooterRows

-- | A 'GetMkFooterRowsConstructor' that outputs one line with the
-- number of objects in the list.
numberOfObjects :: OLS.GetMkFooterRowsConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
numberOfObjects = pure mkfrc
  where
    mkfrc []       = F.noFooterRows
    mkfrc colInfos = Acc.countAccumulator $ mkRows (length colInfos)

    mkRows numCols numObjects = F.rowsOnlyFooter [[numObjectsCell]]
      where
        numObjectsCell = dataCellSpaned (numCols,1) (IntValue numObjects)
