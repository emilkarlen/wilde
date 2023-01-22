{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-------------------------------------------------------------------------------
-- | The object model used by Wilde objects.
--
-- TODO: Remove everything that are utilities.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.ObjectModel
       (

         module Data.Convertible.Base,
         module Data.Typeable,

         module Wilde.Media.WildeMedia,

         -- * Attributes

         AttributeName,
         Any(..),

         -- ** Presentation

         AttributeTypePresentation(..),

         -- ** Attribute Type

         AttributeType(..),

         EmptyAtConfiguration(..),


         -- *** Short cuts

         atConPlainAttr,

         atCrossRefKey_anyValue,

         -- ** Attribute

         Attribute(..),
         AttributeAny(..),

         -- * Object Types and Objects

         AnyO(..),

         -- ** Object Type

         EmptyOtConfiguration(..),

         ObjectType(..),

         ObjectToNativeResult,

         otAttributeTypes,

         ObjectToNativeFunction(..),

         -- ** Object

         Object(..),

         conObject,

         oAllAttributesAnyValue,

         oAttributes,

         oNonIdAttributes,

         -- * Object info for creating Objects
         ObjectForCreate(..),
         AttributeForCreate(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Convertible.Base
import Data.Typeable

import qualified Data.Array.IArray as Array

import qualified Wilde.Database.Sql as Sql

import Wilde.Media.WildeMedia
import Wilde.Media.UserInteraction.Io
import qualified Wilde.Media.Presentation as Presentation


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Presentation -
-------------------------------------------------------------------------------


-- | Presentation (output) functionality of an 'Attribute' and
-- 'AttributeType'.
--
-- By \"presentation\" is menant, primarily, output in a User Interface.
--
-- But a \"presentation media\" could also be something else, e.g. XML.
--
-- Parametrized by
-- a : The type of value that the 'Attribute' (and 'AttributeType') represents.
--
data AttributeTypePresentation a =
  AttributeTypePresentation
  {
    atpoOutput :: PresentationOutputer a
  , atpoTitle  :: StyledTitle
  }


-------------------------------------------------------------------------------
-- - User Interaction -
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- - AttributeType -
-------------------------------------------------------------------------------


-- | An attribute in an 'ObjectType'.
-- An attribute is either a \"plain\" attribute - just data stored in the table.
-- Or it is a reference to another 'ObjectType' (a \"foreign key\" in the database) .
data AttributeType atConf dbTable typeForExisting typeForCreate =
  (Sql.SQL_IDENTIFIER dbTable
  ,Typeable typeForExisting,Show typeForExisting) => AttributeType
  {
    atCrossRefKey   :: AttributeName
  , atPresentationO :: PresentationOutputer typeForExisting
  , atConfiguration :: atConf dbTable typeForExisting typeForCreate
  }

-- | Empty configuration for 'AttributeType'.
data EmptyAtConfiguration dbTable typeForExisting typeForCreate =
  EmptyAtConfiguration

-- | Constructs an 'Attribute' without special presentation information.
atConPlainAttr :: AttributeType atConf dbTable typeForExisting typeForCreate
               -> typeForExisting
               -> Attribute atConf dbTable typeForExisting typeForCreate
atConPlainAttr at@(AttributeType { atPresentationO = presO }) value =
  Attribute
  {
    attrType         = at
  , attrValue        = value
  , attrPresentation = return presentation
   }
  where
    presentation = presO value

-- | Hides the type-for-existing and type-for-create for an 'Attribute'
-- or 'AttributeType'.
--
-- Or, more generally, hides the two last type parameters of a type
-- with three parameters.
data Any t = forall e c . Any (t e c)

-- TODO move to ObjectModelUtils, remove
-- (may be replaced by anyValueECApply...
atCrossRefKey_anyValue :: Any (AttributeType atConf dbTable)
                       -> AttributeName
atCrossRefKey_anyValue (Any at) = atCrossRefKey at


-------------------------------------------------------------------------------
-- - ObjectType -
-------------------------------------------------------------------------------


-- | A type for objects.
-- All attribute types must have the same type of database column names.
-- (Usually one type corresponds to exactly one table.)
data ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate =
  (Sql.SQL_IDENTIFIER dbTable
  ,Typeable idAtExisting,Show idAtExisting) => ObjectType
  {
    otCrossRefKey         :: CrossRefIdentifier
    -- ^ Unique among all OT:s in an app. Used as CGI variable name.

  , otIdAttributeType     :: AttributeType atConf dbTable idAtExisting idAtCreate
    -- ^ The ID-attribute (the PK in the database).

  , otNonIdAttributeTypes :: [Any (AttributeType atConf dbTable)]
    -- ^ Non-ID-attributes.
  , otToNative            :: ObjectToNativeFunction dbTable otNative idAtExisting idAtCreate
    -- ^ Translates an 'Object' to a, perhaps, more convenient type.
  , otConfiguration       :: otConf dbTable otNative idAtExisting idAtCreate
  }

-- | A variant of 'Any' for 'ObjectType's and 'Object's.
data AnyO t = forall dbTable otNative idAtExisting idAtCreate .
              AnyO (t dbTable otNative idAtExisting idAtCreate)

-- | The result of \"tying\" to translate an 'Object' to the \"native\" representation.
type ObjectToNativeResult otNative = Either ObjectToNativeError otNative

-- | Empty configuration for 'ObjectType'.
data EmptyOtConfiguration dbTable otNative idAtExisting idAtCreate =
  EmptyOtConfiguration

data ObjectToNativeFunction dbTable otNative idAtExisting idAtCreate =
  forall otConf atConf .
  ObjectToNativeFunction
  (
    Object otConf atConf dbTable otNative idAtExisting idAtCreate
    -> ObjectToNativeResult otNative
  )

-- | All 'AttributeType's for an 'ObjectType'.
-- The first type is the type for the ID attribute.
--
-- Same as 'otAllAttributeTypes' except for wrapper for hiding the value types.
otAttributeTypes :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> [Any (AttributeType atConf dbTable)]
otAttributeTypes (ObjectType
                  { otIdAttributeType     = theOtIdAttributeType
                  , otNonIdAttributeTypes = theOtNonIdAttributeTypes
                  }) =
      Any theOtIdAttributeType : theOtNonIdAttributeTypes


-------------------------------------------------------------------------------
-- - Attribute -
-------------------------------------------------------------------------------


-- | An instance of an 'AttributeType'.
data Attribute atConf dbTable typeForExisting typeForCreate
  = (Typeable typeForExisting
    ,Show typeForExisting)
    =>
    Attribute
    {
      attrType         :: AttributeType atConf dbTable typeForExisting typeForCreate
    , attrValue        :: typeForExisting
    , attrPresentation :: Presentation.Monad PresentationOutput
    }

data AttributeAny  =
  forall atConf dbTable typeForExisting typeForCreate . (Typeable typeForExisting,Show typeForExisting) =>
  AttributeAny (Attribute atConf dbTable typeForExisting typeForCreate)


-------------------------------------------------------------------------------
-- - Object -
-------------------------------------------------------------------------------


-- | An instance of an 'ObjectType'.
--
-- All attribute types must use the same type for representing
-- database column names.
-- (Usually one type corresponds to exactly one database table.)
data Object otConf atConf dbTable otNative idAtExisting idAtCreate =
  (Sql.SQL_IDENTIFIER dbTable
  ,Typeable idAtExisting
  ,Show idAtExisting)
  =>
  Object
  {
    oType        :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
    -- ^ The type of this object.

  , oIdAttribute :: Attribute atConf dbTable idAtExisting idAtCreate
    -- ^ The ID attribute.

  , oAttributeArray :: Array.Array Int (Any (Attribute atConf dbTable))
    -- ^ All 'Attribute's, indexed 0 ...
    --
    -- First elementis the (mandatory) ID-attribute.
  }

-- | Constructs an 'Object'.
conObject :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> Attribute         atConf dbTable          idAtExisting idAtCreate
          -> [Any (Attribute   atConf dbTable)]
          -> Object     otConf atConf dbTable otNative idAtExisting idAtCreate
conObject ot@(ObjectType {}) idAt nonIdAts =
  Object
  {
    oType            = ot,
    oIdAttribute     = idAt,
    oAttributeArray  = Array.listArray
                       (0,length (otNonIdAttributeTypes ot))
                       (Any idAt : nonIdAts)
  }

oNonIdAttributes :: Object otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> [Any (Attribute atConf dbTable)]
oNonIdAttributes = tail . Array.elems . oAttributeArray

-- | Convenience function that gives a list of all attributes of an 'Object'.
oAllAttributesAnyValue :: Object otConf atConf dbTable otNative idAtExisting idAtCreate
                       -> [Any (Attribute atConf dbTable)]
oAllAttributesAnyValue = Array.elems . oAttributeArray

-- | Convenience function that gives a list of all attributes of an 'Object'.
oAttributes :: Object otConf atConf dbTable otNative idAtExisting idAtCreate
            -> [Any (Attribute atConf dbTable)]
oAttributes o@(Object {}) = Any (oIdAttribute o) : oNonIdAttributes o


-------------------------------------------------------------------------------
-- - ForCreate -
-------------------------------------------------------------------------------


-- | An attribute of an object that has not yet been created.
data AttributeForCreate atConf dbTable typeForExisting typeForCreate
  = (Sql.SQL_IDENTIFIER dbTable
    ,Typeable typeForExisting
    ,Show typeForExisting)
    =>
    AttributeForCreate
    {
      attrfcType  :: AttributeType atConf dbTable typeForExisting typeForCreate
    , attrfcValue :: typeForCreate
    }

-- | A type for objects that has not yet been created.
--
-- All values are of the type typeForCreate, instead of the type typeForExisting.
data ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate =
  (Sql.SQL_IDENTIFIER dbTable
  ,Typeable idAtExisting
  ,Show idAtExisting)
  =>
  ObjectForCreate
  {
    ofcType            :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
    -- ^ The type of this object.

  , ofcIdAttribute     :: AttributeForCreate atConf dbTable idAtExisting idAtCreate
    -- ^ The ID attribute.

  , ofcNonIdAttributes :: [Any (AttributeForCreate atConf dbTable)]
    -- ^ Non-ID attributes.
  }
