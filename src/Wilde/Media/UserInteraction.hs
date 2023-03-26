{-# LANGUAGE ExistentialQuantification #-}

-- | Types related to User Interaction/User Interaface,
-- and basic operations on these types.
module Wilde.Media.UserInteraction
       (
         -- * Names

         ObjectName,
         AttributeName,
         objectName,
         subObjectName,
         objectNameAsElementKey,
         attributeName,
         attributeElementKey,

         -- * Element

         module Wilde.Media.Element,
         Monoid(..),

         -- * Widgets and forms

         Label(..),

         -- ** Widget

         WIDGET(..),
         AnyWIDGET(..),

         -- * Forms

         GenericWidgetDefaultValue,
         MultiItem,
         MultiItemPresentation,
         MultiWidgetConstructor,

         FormAction,

         -- ** Form Block

         FormBlock(..),

         formBlockAppendMetaValues,

         -- *** FormBlockRow

         FormBlockRow,

         attributeOutputFormBlockRow,
         presentationOutputFormBlockRow,

         -- *** FormBlockRowInfo

         FormBlockRowInfo(..),
         concatAtFormBlockInfos,
         formBlockInfoAsFormBlock,
         mkFormBlockRowInfoForLabelAndWidget,
         mkFormBlockRowInfoForMetas,
         mkFormBlockRowInfoForPresentation,

         -- ** Container for Form Blocks and Meta Elements

         FormBlocksAndMetas(..),
         fbamAppendMetas,
         fbamAppendBlocks,

         -- ** Form

         Form(..),
         formAppendMetaValues,
         formForBlock,
         formForFormBlocksAndMetas,
         formForFormBlocksAndMetasForSameProgram,

         -- * Attribute Output

         FormBlockOutputer,
         FormBlockRowOutputer,
         formBlockFromRows,
         LabelAndWidget(..),
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Maybe (maybeToList)

import           Wilde.Utils.Empty

import           Wilde.GenericUi.Widget

import           Wilde.WildeUi.UiPrimitives

import           Wilde.Media.WildeMedia
import           Wilde.Media.Element
import           Wilde.Media.GenericStringRep


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - names -
-------------------------------------------------------------------------------


-- | Name of an object that can contain attributes.
--
-- The object name becomes the prefix of it's attributes.
type ObjectName = ElementKeyPrefix

-- | Name of an 'Attribute'.
type AttributeName = ElementKeyLeaf -- CrossRefIdentifier

-- | A "top level" 'ObjectName' - a name that consist of a single part.
objectName :: CrossRefIdentifier -> ObjectName
objectName = elementKeyPrefixFromString

-- | A "top level" 'ObjectName' - a name that consist of a single part.
attributeName :: CrossRefIdentifier -> AttributeName
attributeName = id

subObjectName :: ObjectName -> AttributeName -> ObjectName
subObjectName = elementKeyPrefixAdd

objectNameAsElementKey :: ObjectName -> ElementKey
objectNameAsElementKey = elementKeyPrefixAsKey

attributeElementKey :: ObjectName -> AttributeName -> ElementKey
attributeElementKey = elementKey


-------------------------------------------------------------------------------
-- - LabelAndWidget -
-------------------------------------------------------------------------------


-- | Information for outputing an attribute /to/ a 'Form' or 'FormBlock'.
type LabelAndWidget = (Label,AnyWIDGET)


-------------------------------------------------------------------------------
-- - Widget -
-------------------------------------------------------------------------------


-- | A generic way of specifying a default value for a widget.
--
-- Ideally, any type of widget should understand strings, possibly
-- formatted according to what type of information the widget handles.
-- (E.g. integer, double, date).
type GenericWidgetDefaultValue = String

-- Each value is identified by a key, and presented via a presentation-string.
type MultiWidgetConstructor = Maybe GenericStringRep -- ^ pre-selected
                              -> CrossRefIdentifier -- ^ Attribute name
                              -> [MultiItem]        -- ^ Values (key,presentation string)
                              -> ElementKeyPrefix   -- ^ Object name
                              -> AnyWIDGET

-- | For web: the URL of a form.
type FormAction = String


-------------------------------------------------------------------------------
-- - FormBlock -
-------------------------------------------------------------------------------


-- | A rectangular block of a 'Form'.
--
-- A container of interaction and meta values, so that these can be pureed
-- as a unit.
--
data FormBlock = FormBlock
                 {
                   formBlockInteraction :: [FormBlockRow],
                   formBlockMetaValues  :: [Element]
                 }
instance Semigroup FormBlock where
  fp1 <> fp2 = FormBlock
               { formBlockInteraction = concatMap formBlockInteraction [fp1,fp2]
               , formBlockMetaValues  = concatMap formBlockMetaValues  [fp1,fp2]
               }


instance Monoid FormBlock where
  mempty = FormBlock [] []


-- | One row of a 'FormBlock'.
type FormBlockRow = Either
                    LabelAndWidget
                    (Title,PresentationOutput)

attributeOutputFormBlockRow :: LabelAndWidget -> FormBlockRow
attributeOutputFormBlockRow = Left

presentationOutputFormBlockRow :: (Title,PresentationOutput) -> FormBlockRow
presentationOutputFormBlockRow = Right

-- | Append 'Element's to the Meta Element's of a given 'FormBlock'.
formBlockAppendMetaValues :: FormBlock -> [Element] -> FormBlock
formBlockAppendMetaValues f@(FormBlock _ currentMetas) newMetas =
  f { formBlockMetaValues = currentMetas ++ newMetas }


-------------------------------------------------------------------------------
-- - FormBlockRowInfo -
-------------------------------------------------------------------------------


-- | Information about how to use an 'AttributeType'
-- in a 'FormBlock'.
data FormBlockRowInfo =
  FormBlockRowInfo
  {
    atFormBlockMetaValues :: [Element]
  , atFormBlockRow        :: Maybe FormBlockRow
  }

concatAtFormBlockInfos :: [FormBlockRowInfo]
                       -> FormBlock
concatAtFormBlockInfos = mconcat . map formBlockInfoAsFormBlock

formBlockInfoAsFormBlock :: FormBlockRowInfo
                         -> FormBlock
formBlockInfoAsFormBlock atFbInfo@(FormBlockRowInfo {
                                      atFormBlockMetaValues = theMetaValues,
                                      atFormBlockRow        = mbFormBlockRow
                                      })
  =
  FormBlock formBlockRows theMetaValues
  where
    formBlockRows = maybeToList mbFormBlockRow

instance EMPTY FormBlockRowInfo where
  empty =
    FormBlockRowInfo
    {
      atFormBlockMetaValues = []
    , atFormBlockRow        = Nothing
    }

mkFormBlockRowInfoForLabelAndWidget :: LabelAndWidget
                                    -> FormBlockRowInfo
mkFormBlockRowInfoForLabelAndWidget x =
  FormBlockRowInfo
  {
    atFormBlockMetaValues = []
  , atFormBlockRow        = Just $ attributeOutputFormBlockRow x
  }

mkFormBlockRowInfoForPresentation :: (Title,PresentationOutput)
                                  -> FormBlockRowInfo
mkFormBlockRowInfoForPresentation x =
  FormBlockRowInfo
  {
    atFormBlockMetaValues = []
  , atFormBlockRow        = Just $ Right x
  }

mkFormBlockRowInfoForMetas :: [Element]
                           -> FormBlockRowInfo
mkFormBlockRowInfoForMetas x =
  FormBlockRowInfo
  {
    atFormBlockMetaValues = x
  , atFormBlockRow        = Nothing
  }

-------------------------------------------------------------------------------
-- - FormBlocksAndMetas -
-------------------------------------------------------------------------------


data FormBlocksAndMetas =
  FormBlocksAndMetas
  {
    fbamMetas  :: [Element],
    fbamBlocks :: [FormBlock]
  }

instance Semigroup FormBlocksAndMetas where
  (FormBlocksAndMetas mL bL) <> (FormBlocksAndMetas mR bR) =
    FormBlocksAndMetas (mL <> mR) (bL <> bR)

instance Monoid FormBlocksAndMetas where
  mempty = FormBlocksAndMetas [] []

instance EMPTY FormBlocksAndMetas where
  empty = FormBlocksAndMetas [] []

-- | Append 'Element's to the Meta Element's of a given 'FormBlocksAndMetas'.
fbamAppendMetas :: FormBlocksAndMetas -> [Element] -> FormBlocksAndMetas
fbamAppendMetas x@(FormBlocksAndMetas currentMetas _) newMetas =
  x { fbamMetas = currentMetas ++ newMetas }

-- | Adds two 'FormBlocksAndMetas's component wise.
fbamAppendBlocks :: FormBlocksAndMetas -> [FormBlock] -> FormBlocksAndMetas
fbamAppendBlocks x@(FormBlocksAndMetas _ currentBlocks) newBlocks =
  x { fbamBlocks = currentBlocks ++ newBlocks }


-------------------------------------------------------------------------------
-- - Form -
-------------------------------------------------------------------------------


-- | A \"form\" represents information that the user interacts with
-- via a User Interface.
--
-- A \"page\" can contain many forms, but only one of them can be processed
-- by an application per \"request\".  (Thus, if the page contains many forms,
-- the user has to select which one to process.)
data Form = Form
            {
              -- | Information in the form that is invisible to the user.
              formMetaValues  :: [Element],

              -- | The things in the form that is visible to the user.
              formBlocks :: [FormBlock],

              -- | Defines \"who\" should process the form.

              -- Mer specifik typ här??
              -- Skulle va tjusigt om en tjänst = hs-metod kunde anges här.
              -- Ännu mer om det kunde typas!
              formAction      :: Maybe FormAction
            }

-- | Constructs a 'Form' for a 'FormBlock' and a 'FormAction'.
formForBlock :: FormBlock -> Maybe FormAction -> Form
formForBlock formBlock formAction = Form
                    {
                      formMetaValues  = [],
                      formAction      = formAction,
                      formBlocks      = [formBlock]
                     }

-- | Append 'Element's to the Meta Element's of a given 'Form'.
formAppendMetaValues :: Form -> [Element] -> Form
formAppendMetaValues f@(Form currentMetas _ _) newMetas =
  f { formMetaValues = currentMetas ++ newMetas }

-- | Constructs a 'Form' from a 'formBlocksAndMetas'.
formForFormBlocksAndMetas :: FormBlocksAndMetas
                             -> [Element] -- ^ Meta values (in addition to those
                                          -- in the 'FormBlocksAndMetas's)
                             -> Maybe FormAction
                             -> Form
formForFormBlocksAndMetas formBlocksAndMetas formMetas formAction = Form
                    {
                      formMetaValues  = formMetas ++ fbamMetas formBlocksAndMetas,
                      formAction      = formAction,
                      formBlocks      = fbamBlocks formBlocksAndMetas
                     }

-- | Constructs a 'Form' from a 'formBlocksAndMetas' that invokes
-- the same CGI program that is calling this method.
formForFormBlocksAndMetasForSameProgram :: FormBlocksAndMetas
                                        -> [Element] -- ^ Meta values (in addition to those
                                                     -- in the 'FormBlocksAndMetas's)
                                        -> Form
formForFormBlocksAndMetasForSameProgram formBlocksAndMetas formMetas =
  Form
  {
    formMetaValues  = formMetas ++ fbamMetas formBlocksAndMetas,
    formAction      = Nothing,
    formBlocks      = fbamBlocks formBlocksAndMetas
  }

formBlockFromRows :: [FormBlockRowOutputer defaultType]
                  -> FormBlockOutputer defaultType
formBlockFromRows rowOutputers defaltValue objectName =
  concatAtFormBlockInfos $
  map (\rowOutputer -> rowOutputer defaltValue objectName) rowOutputers


-------------------------------------------------------------------------------
-- - outputers -
-------------------------------------------------------------------------------


type FormBlockOutputer defaultType =
  Maybe defaultType -> ObjectName -> FormBlock


type FormBlockRowOutputer defaultType =
  Maybe defaultType -> ObjectName -> FormBlockRowInfo
