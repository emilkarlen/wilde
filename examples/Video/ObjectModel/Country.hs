module Video.ObjectModel.Country
       (
         Table,
         NativeType,
         rps,
         ots',
       )
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Common.ObjectType.IdName as IdName


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type Table      = IdName.Table
type NativeType = IdName.IdNameType

(rps, ots') = IdName.rps_and_ots_stdPkName "Land" "country"
