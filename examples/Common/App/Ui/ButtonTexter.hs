module ButtonTexter 
       (
         buttonText,
       )
       where

import qualified Wilde.Application.Service.PopUp as PopUp

buttonText :: PopUp.Button -> String
buttonText PopUp.Ok  = "OK"
buttonText PopUp.Yes = "Ja"
buttonText PopUp.No  = "Nej"
