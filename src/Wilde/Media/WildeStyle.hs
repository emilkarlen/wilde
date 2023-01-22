-- | The \"standard\" styles used by Wilde.
module Wilde.Media.WildeStyle where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.GenericUi.Style
import Wilde.Media.WildeStyleType


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


helpClass,varnameClass,varFormatClass :: ClassName
helpClass      = "_help"
varnameClass   = "_varname"
varFormatClass = "_varformat"

weObjectClass,weAttributeClass :: ClassName
weObjectClass    = "_weO"
weAttributeClass = "_weA"

weObject,weAttribute :: WildeStyle
weObject    = WildeStyle [weObjectClass]
weAttribute = WildeStyle [weAttributeClass]

-- | Component classes.
pageClass,componentClass :: ClassName
pageClass      = "_cPg" -- "Page" class.
componentClass = "_cC"  -- "Component" class.

presentationClass,inputClass :: ClassName
presentationClass = "_uP"
inputClass        = "_uI"

-- | Multiplicity classes.
singleClass,multiClass :: ClassName
singleClass       = "_m1"
multiClass        = "_mN"
multiEmptyClass   = "_mN0"

-- Sequence Position
spEven,spOdd :: ClassName
spEven            = "_spE"
spOdd             = "_spO"

-- | Role classes.
titleClass,labelClass,valueClass,sumClass :: ClassName
titleClass        = "_rT"
labelClass        = "_rL"
valueClass        = "_rV"
sumClass          = "_summary"
commentClass      = "_comment"

commentStyle :: WildeStyle
commentStyle = WildeStyle [commentClass]

objectButtonClass :: ClassName
objectButtonClass = "_a"

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
objectButtonStyle = WildeStyle [objectButtonClass]

tableListBodyRowGroup :: WildeStyle
tableListBodyRowGroup = WildeStyle [valueClass]

presentationTableMulti :: WildeStyle
presentationTableMulti = WildeStyle (weObjectClass : componentClass : presMultiClasses)

presentationTableSingle :: WildeStyle
presentationTableSingle = WildeStyle (weObjectClass : componentClass : presSingleClasses)

userInteractionTable :: WildeStyle
userInteractionTable = WildeStyle (weObjectClass : componentClass : inputSingleClasses)

multiColumnTitle :: WildeStyle
multiColumnTitle = WildeStyle [titleClass]

attributeTitle :: WildeStyle
attributeTitle = WildeStyle [weAttributeClass,titleClass]

multiRowEven,multiRowOdd :: WildeStyle
multiRowEven = WildeStyle [weObjectClass,spEven]
multiRowOdd  = WildeStyle [weObjectClass,spOdd]

pageTitleClasses,componentTitleClasses :: [ClassName]
pageTitleClasses      = [pageClass,titleClass]
componentTitleClasses = [componentClass,titleClass]

pageTitle,componentTitle :: WildeStyle
pageTitle      = WildeStyle pageTitleClasses
componentTitle = WildeStyle componentTitleClasses

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
