-------------------------------------------------------------------------------
-- | Structures for small dialogs, with simple questions - "pop ups".
-------------------------------------------------------------------------------
module Wilde.Application.Service.PopUp
       (
         Message,
         Type,
         Button(..),
       )
       where


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type Message = String

data Type = YesNo

data Button
  = Ok
  | Yes
  | No
  | Reset
