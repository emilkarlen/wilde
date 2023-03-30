-- | The \"standard\" styles used by Wilde.
module Wilde.WildeUi.WildeStyles where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.WildeUi.WildeStyle


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


helpClass,varnameClass,varFormatClass :: ClassName
helpClass      = "_help"
varnameClass   = "_varname"
varFormatClass = "_varformat"

-- | Result of the "request"
resSrvcOk, resSrvcError, resSrvcPopUp_askIfContinue, resSrvcPopUp_info :: ClassName
resSrvcOk                  = "_resService_ok"
resSrvcError               = "_resService_error"
resSrvcPopUp_askIfContinue = "_resService_popup_askIfContinue"
resSrvcPopUp_info          = "_resService_popup_info"

-- | Components - different parts of the whole page/screen.
compPageClass,compComponentClass :: ClassName
compPageClass      = "_cPg" -- "Page" - the whole "screen".
compComponentClass = "_cC"  -- "Component" a block with a special purpose

-- Wilde Elements
weObjectClass,weAttributeClass :: ClassName
weObjectClass    = "_weO"
weAttributeClass = "_weA"

weObject,weAttribute :: WildeStyle
weObject    = WildeStyle [weObjectClass]
weAttribute = WildeStyle [weAttributeClass]

subObjectListClass :: ClassName
subObjectListClass = "_rel_dependent_component"

-- UI type of output
presentationClass,inputClass :: ClassName
presentationClass = "_uP"
inputClass        = "_uI"

-- | Multiplicity classes.
singleClass,multiClass :: ClassName
singleClass       = "_m1"
multiClass        = "_mN"
multiEmptyClass   = "_mN0"

-- | Role classes.
titleClass,labelClass,valueClass,actionsClass, sumClass :: ClassName
titleClass        = "_rT" -- ^ title of page, component
labelClass        = "_rL" -- ^ label of field
valueClass        = "_rV"
actionsClass      = "_rA"
sumClass          = "_summary"
commentClass      = "_comment"

commentStyle :: WildeStyle
commentStyle = WildeStyle [commentClass]


buttonClass, imageClass,textClass :: ClassName
buttonClass = "_btn"
imageClass  = "_img"
textClass   = "_txt"

referenceAttribute :: ClassName
referenceAttribute = "_refattr"

errorClass :: ClassName
errorClass = "_error"

errorStyle :: WildeStyle
errorStyle = WildeStyle [errorClass]

imageButtonStyle,textButtonStyle :: WildeStyle
imageButtonStyle = wildeClassStyle [buttonClass, imageClass]
textButtonStyle  = wildeClassStyle [buttonClass, textClass]

referenceAttributeStyle :: WildeStyle
referenceAttributeStyle = WildeStyle [referenceAttribute]

objectButtonStyle :: WildeStyle
objectButtonStyle = WildeStyle [actionsClass]

tableListBodyRowGroup :: WildeStyle
tableListBodyRowGroup = WildeStyle [valueClass]

presentationTableMulti :: WildeStyle
presentationTableMulti = WildeStyle (weObjectClass : compComponentClass : presMultiClasses)

presentationTableSingle :: WildeStyle
presentationTableSingle = WildeStyle (weObjectClass : compComponentClass : presSingleClasses)

userInteractionTable :: WildeStyle
userInteractionTable = WildeStyle (weObjectClass : compComponentClass : inputSingleClasses)

multiColumnTitle :: WildeStyle
multiColumnTitle = WildeStyle [titleClass]

attributeTitle :: WildeStyle
attributeTitle = WildeStyle [weAttributeClass,titleClass]

multiRow :: WildeStyle
multiRow = WildeStyle [weObjectClass]

pageTitleClasses,componentTitleClasses :: [ClassName]
pageTitleClasses      = [compPageClass,titleClass]
componentTitleClasses = [compComponentClass,titleClass]

pageTopLevelComponentClasses :: [ClassName]
pageTopLevelComponentClasses = [compPageClass, compComponentClass]
-- pageTopLevelComponentClasses = ["_develop__top_level_component"]

pageTitle,componentTitle :: WildeStyle
pageTitle      = WildeStyle pageTitleClasses
componentTitle = WildeStyle componentTitleClasses

actionsComponentClasses :: [ClassName]
actionsComponentClasses = [compComponentClass, actionsClass]

presMultiClasses,presMultiValueClasses,presMultiLabelClasses :: [ClassName]
presMultiClasses      = [presentationClass,multiClass]
presMultiValueClasses = valueClass : presMultiClasses
presMultiLabelClasses = titleClass : presMultiClasses

presSingleClasses,presSingleValueClasses,presSingleLabelClasses :: [ClassName]
presSingleClasses      = [presentationClass,singleClass]
presSingleValueClasses = valueClass : presSingleClasses
presSingleLabelClasses = titleClass : presSingleClasses

presMultiValueStyle,presMultiLabelStyle :: WildeStyle
presMultiValueStyle = WildeStyle presMultiValueClasses
presMultiLabelStyle = WildeStyle presMultiLabelClasses

inputSingleClasses,inputSingleValueClasses,inputSingleLabelClasses :: [ClassName]
inputSingleClasses      = [inputClass,singleClass]
inputSingleValueClasses = valueClass : inputSingleClasses
inputSingleLabelClasses = titleClass : inputSingleClasses

sumStyle :: WildeStyle
sumStyle = WildeStyle [sumClass]

tableColumnStylesShowOne :: (WildeStyle,WildeStyle)
tableColumnStylesShowOne = (WildeStyle [presentationClass,titleClass],
                            WildeStyle [presentationClass,valueClass])

tableColumnStylesInputOne :: (WildeStyle,WildeStyle)
tableColumnStylesInputOne = (WildeStyle [inputClass,titleClass],
                             WildeStyle [inputClass,valueClass])
