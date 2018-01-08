module Common.ObjectType.ManyToMany
       (
       )
       where


import           Wilde.ApplicationConstruction.ObjectModel.StandardPrimaryKey
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType



data Table = Id
           | Name
           | Country_id
           | Url
           deriving Show

instance SQL_IDENTIFIER Table where
  sqlIdentifier x = show x

type NativeType = (PrimaryKeyType, PrimaryKeyType)

type OType t = Std_ddl t Table NativeType

type OObjectType = OType ObjectType
type OObject     = OType Object


