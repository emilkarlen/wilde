-- | The \"Generic String Representation\" is a media where a value
-- can converted to/from a String, in much the same manner as Haskell's
-- Show/Read classes.
module Wilde.Media.GenericStringRep
       (
         emptyValue,

         GenericStringRep,
         GenericStringRepInputer,
         GenericStringRepOutputer,
         GenericStringRepIo(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.ElementSet


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A media for use in User Interaction, but that is not aimed to be
-- visible for the user.
--
-- This media is used for passing attribute values around
-- \"behind the scenes\".  An example is the values of a reference
-- attribute stored for each option in a drop down widget.
type GenericStringRep = String

emptyValue :: GenericStringRep
emptyValue = ""

type GenericStringRepInputer a =
  GenericStringRep -> Either ElementLookupErrorType a

type GenericStringRepOutputer a = a -> GenericStringRep

data GenericStringRepIo a =
  GenericStringRepIo
  {
    gsrInputer  :: GenericStringRepInputer  a
  , gsrOutputer :: GenericStringRepOutputer a
   }
