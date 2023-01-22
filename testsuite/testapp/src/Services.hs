module Services where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as Map

import Wilde.ObjectModel.ObjectModel

import Wilde.Service.Monad

import Wilde.ApplicationConstruction.StandardServices

import Wilde.ApplicationConstruction.Service.StepService

import Wilde.ApplicationConstruction.Database.DatabaseUtils

import qualified ObjectModel

import Wilde.Application.Service.Service


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
