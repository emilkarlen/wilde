module Wilde.Application.Service.Service
(
    module Wilde.Application.Service.Result,

    Service,

    pageOkResult,
    popupOkResult,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Service.Monad (ServiceMonad)
import Wilde.Application.Service.Result


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type Service = ServiceMonad ServiceOkResult


-- | Makes the given page the result of the service.
pageOkResult :: ServicePage -> ServiceMonad ServiceOkResult
pageOkResult page = pure $ OkResultPage page

-- | Makes the given popup the result of the service.
popupOkResult :: ServicePopUp -> ServiceMonad ServiceOkResult
popupOkResult popup = pure $ OkResultPopUp popup
