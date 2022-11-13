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

module Services where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as Map

import Wilde.ObjectModel.ObjectModel

import Wilde.Application.Service

import Wilde.ApplicationConstruction.StandardServices

import Wilde.ApplicationConstruction.Service.StepService

import Wilde.ApplicationConstruction.Database.DatabaseUtils

import qualified ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


services :: ApplicationServices
services = Map.union globalServices otServices

otServices :: ApplicationServices
otServices = standardServices $ map anyOWithDdlInfo2AnyO ObjectModel.objectModel

globalServices :: ApplicationServices
globalServices = Map.fromList
  [
    ("ok"          ,mkGlobalService $ pageOkResult (withNeutralWildeStyle "Just OK",[]))
  , ("error"       ,mkGlobalService $ throwErr (unclassifiedError "An expected error"))
  , ("ask-if-cont" ,mkGlobalService $ askIfContTest)
  ]

askIfContTest = stepService config
  where
    config = StepService
           {
             mainTitle    = withNeutralWildeStyle "Ask If Continue"
           , nonLastSteps = [askIfContinue "Almost there, just one step more."]
           , lastStep     = pageOkResult (withNeutralWildeStyle "The END",[])
           }
