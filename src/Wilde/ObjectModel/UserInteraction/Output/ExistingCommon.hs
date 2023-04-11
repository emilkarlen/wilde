-- | Utilities related to outputers of existing objects.

{-# LANGUAGE ExistentialQuantification #-}

module Wilde.ObjectModel.UserInteraction.Output.ExistingCommon
(
  AttributeUiDefaultForExisting,
  UserInteractionOutputerForExisting,

  ATTRIBUTE_OUTPUT_FOR_EXISTING(..),
  AttributeTypeInfo(..),

  AttributeInfo(..),

  at2ati,
  attr2attri,

  getMkAttributeOutputFun,

  -- * Configuration of the input form

  AttributeTypeConfiguration(..),
  AttributeTypeConfigurations,

  -- ** The role of an 'AttributeType' in an input form.

  AttributeTypeRole(..),
  atConfigIs,
  atsWith,

  -- * Re-exporting types used here

  WildeTitle,
  PresentationOutputer(..),
  AttributeName,
  UiO.WidgetConstructorGetter(..),
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


--import Wilde.Media.UserInteraction.Io
import qualified Wilde.Media.UserInteraction.Output as UiO

import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.UserInteraction
import           Wilde.ObjectModel.Presentation (ATTRIBUTE_PRESENTATION(..))

import           Wilde.WildeUi.UiPrimitives


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Information about an 'AttributeType' that makes it possible to
-- construct an UI outputer for it.
data AttributeTypeInfo typeForExisting =
  Typeable typeForExisting =>
  AttributeTypeInfo
  {
    atiCrossRefKey              :: AttributeName
  , atiTitle                    :: WildeTitle
  , atiPresentationO            :: PresentationOutputer               typeForExisting
  , atiOutputerForAttributeName :: UserInteractionOutputerForExisting typeForExisting
  }

-- | Extracts the information we need about an 'AttributeType'.
at2ati :: ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
       => AttributeType atConf dbTable typeForExisting typeForCreate
       -> AttributeTypeInfo            typeForExisting
at2ati at@(AttributeType {
              atCrossRefKey   = theCrossRefKey,
              atPresentationO = thePresO })
  =
  AttributeTypeInfo
  {
    atiCrossRefKey              = theCrossRefKey
  , atiTitle                    = atTitle at
  , atiPresentationO            = thePresO
  , atiOutputerForAttributeName = atOutputerForExisting at
  }

-- | Information about an 'Attribute' that makes it possible to
-- construct an UI outputer for it.
data AttributeInfo a =
  Typeable a => AttributeInfo
  {
    aiCrossRefKey :: AttributeName
  , aiValue       :: a
  }

-- | Extracts 'AttributeInfo' from an 'Attribute'.
attr2attri :: Attribute atConf dbTable typeForExisting typeForCreate
           -> AttributeInfo            typeForExisting
attr2attri (Attribute
            {
              attrValue = theValue
            , attrType  = theType
            }) =
    AttributeInfo
    {
      aiCrossRefKey = atCrossRefKey theType
    , aiValue       = theValue
    }


------------------------------------------------------------------------------
-- Output, for a maybe existing value, of a single 'AttributeTypeInfo'.
-------------------------------------------------------------------------------
getMkAttributeOutputFun :: (AttributeTypeRole,AttributeTypeInfo a)
                        -> UiO.Monad
                           (Maybe a -> UiO.ObjectName -> UiO.FormBlockRow)
getMkAttributeOutputFun
  ( UserInteraction,
    AttributeTypeInfo
    {
      atiCrossRefKey              = theCrossRefKey
    , atiOutputerForAttributeName = theOutputerGetter
    , atiTitle                    = theTitle
    }
  )
  =
  do
    getWidget <- theOutputerGetter theCrossRefKey
    pure $
      \mbDefault objectName ->
      let
        ek     = (objectName,theCrossRefKey)
        label  = UiO.Label ek (wildeStyled theTitle)
        widget = getWidget mbDefault objectName
      in
       Left (label,widget)

getMkAttributeOutputFun (_,
  AttributeTypeInfo
  {
    atiPresentationO = presOutputer
  , atiTitle         = titleWithStyle
  })
  =
    pure $
      \mbValue objectName -> Right (wildeStyled titleWithStyle,
                                    maybe empty presOutputer mbValue)



-- | The role of an 'AttributeType' in
-- an input form.
data AttributeTypeRole
  = UserInteraction
    -- ^ Widget for user input
  | Presentation
    -- ^ Just displays the value - no input
  deriving (Eq,Enum,Show,Read)

data AttributeTypeConfiguration atConf dbTable =
  AttributeTypeConfiguration
  {
    configRole :: AttributeTypeRole
  , configAt   :: Any (AttributeType atConf dbTable)
  }

-------------------------------------------------------------------------------
-- | Configuration of the usage of 'AttributeType's in the input form.
--
-- The order of the types in this list determines the order
-- they are displayed in the form.
--
-- Each 'AttributeType' may appear zero or one time in the list.
-- If it appears zero times, then it will not be displayed in the form.
-------------------------------------------------------------------------------
type AttributeTypeConfigurations atConf dbTable = [AttributeTypeConfiguration atConf dbTable]

-- | Predicate for for
atConfigIs :: AttributeTypeRole
           -> AttributeTypeConfiguration atConf dbTable
           -> Bool
atConfigIs role = (==role) . configRole

-- | Extraction of 'AttributeType's for a given 'AttributeTypeRole'.
atsWith :: AttributeTypeRole
        -> AttributeTypeConfigurations atConf dbTable
        -> [Any (AttributeType  atConf dbTable)]
atsWith role = map configAt . filter (atConfigIs role)
