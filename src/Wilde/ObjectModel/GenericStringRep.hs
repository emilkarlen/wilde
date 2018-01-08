{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | Functionality related to the \"Generic String Representation\"
-- format.
module Wilde.ObjectModel.GenericStringRep
       (
         module Wilde.Media.GenericStringRep,
         module Wilde.ObjectModel.ObjectModel,

         -- * Information on 'AttributeType's.

         ATTRIBUTE_INPUT_FOR_CREATE(..),
         ATTRIBUTE_OUTPUT_FOR_CREATE(..),
         ATTRIBUTE_IO_FOR_CREATE(..),

         ATTRIBUTE_INPUT_FOR_EXISTING(..),
         ATTRIBUTE_OUTPUT_FOR_EXISTING(..),
         ATTRIBUTE_IO_FOR_EXISTING(..),

         -- * Short cuts

         inputer,
         outputer,

         -- * For 'AttributeType' and 'Attribute'

         attrOutput,

         -- * For 'ObjectType' and 'Object'

         otIoForIdAtForExisting,
         otInputerForIdAtForExisting,
         otOutputerForIdAtForExisting,

         objOutputForIdAt,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Convertible.Base
import Data.Typeable

import Wilde.Media.WildeMedia
import Wilde.Media.GenericStringRep

import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


class ATTRIBUTE_INPUT_FOR_CREATE atConf where
  atInputerForCreate :: AttributeType atConf dbTable typeForExisting typeForCreate
                      -> GenericStringRepInputer                     typeForCreate

class ATTRIBUTE_OUTPUT_FOR_CREATE atConf where
  atOutputerForCreate :: AttributeType atConf dbTable typeForExisting typeForCreate
                      -> GenericStringRepOutputer                     typeForCreate


class ATTRIBUTE_OUTPUT_FOR_EXISTING atConf where
  atOutputerForExisting :: AttributeType atConf dbTable typeForExisting typeForCreate
                        -> GenericStringRepOutputer     typeForExisting

class ATTRIBUTE_INPUT_FOR_EXISTING atConf where
  atInputerForExisting :: AttributeType atConf dbTable typeForExisting typeForCreate
                       -> GenericStringRepInputer      typeForExisting

class (ATTRIBUTE_INPUT_FOR_CREATE atConf
      ,ATTRIBUTE_OUTPUT_FOR_CREATE atConf)
      =>
      ATTRIBUTE_IO_FOR_CREATE atConf where
  atIoForCreate :: AttributeType atConf dbTable typeForExisting typeForCreate
                -> GenericStringRepIo typeForCreate
  atIoForCreate at =
    GenericStringRepIo
    {
      gsrInputer  = atInputerForCreate at
    , gsrOutputer = atOutputerForCreate at
    }

class (ATTRIBUTE_INPUT_FOR_EXISTING atConf
      ,ATTRIBUTE_OUTPUT_FOR_EXISTING atConf)
      =>
      ATTRIBUTE_IO_FOR_EXISTING atConf where
  atIoForExisting :: AttributeType atConf dbTable typeForExisting typeForCreate
                  -> GenericStringRepIo typeForExisting
  atIoForExisting at =
    GenericStringRepIo
    {
      gsrInputer  = atInputerForExisting at
    , gsrOutputer = atOutputerForExisting at
    }


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


inputer :: GenericStringRepIo a -> GenericStringRepInputer  a
inputer = gsrInputer

outputer :: GenericStringRepIo a -> GenericStringRepOutputer  a
outputer = gsrOutputer


-------------------------------------------------------------------------------
-- - AttributeType -
-------------------------------------------------------------------------------


attrOutput :: ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
           => Attribute atConf dbTable idAtExisting idAtCreate
           -> GenericStringRep
attrOutput attr = outputer $ attrValue attr
  where
    outputer = atOutputerForExisting (attrType attr)


-------------------------------------------------------------------------------
-- - ObjectType -
-------------------------------------------------------------------------------


otIoForIdAtForExisting :: ATTRIBUTE_IO_FOR_EXISTING atConf
                       => ObjectType otConf atConf dbTable otNative idAtExisting idAtForCreate
                       -> GenericStringRepIo idAtExisting
otIoForIdAtForExisting = atIoForExisting . otIdAttributeType

otInputerForIdAtForExisting :: ATTRIBUTE_INPUT_FOR_EXISTING atConf
                            => ObjectType otConf atConf dbTable otNative idAtExisting idAtForCreate
                            -> GenericStringRepInputer idAtExisting
otInputerForIdAtForExisting = atInputerForExisting . otIdAttributeType

otOutputerForIdAtForExisting :: ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                             => ObjectType otConf atConf dbTable otNative idAtExisting idAtForCreate
                             -> GenericStringRepOutputer idAtExisting
otOutputerForIdAtForExisting = atOutputerForExisting . otIdAttributeType

objOutputForIdAt :: ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                 => Object otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> GenericStringRep
objOutputForIdAt o = outputer $ attrValue idAttr
  where
    outputer = atOutputerForExisting (attrType idAttr)
    idAttr = oIdAttribute o
