-- | CGI values and tools.
module Wilde.Driver.Application.Cgi.VariableNames
       (

         customEnvironment,
         objectType,
         service,
         pk,

         pretty,

         selectExpression,
       )
       where


customEnvironment,objectType,service,pk :: String
customEnvironment = "_env"
objectType        = "_ot"
service           = "_srvc"
pk                = "_pk"

pretty :: String
pretty     = "_pretty"

selectExpression :: String
selectExpression = "_selection"
