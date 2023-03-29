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

-- Wilde Elements
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

subObjectListClass :: ClassName
subObjectListClass = "_rl_dependent_component"

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
presentationTableMulti = WildeStyle (weObjectClass : componentClass : presMultiClasses)

presentationTableSingle :: WildeStyle
presentationTableSingle = WildeStyle (weObjectClass : componentClass : presSingleClasses)

userInteractionTable :: WildeStyle
userInteractionTable = WildeStyle (weObjectClass : componentClass : inputSingleClasses)

multiColumnTitle :: WildeStyle
multiColumnTitle = WildeStyle [titleClass]

attributeTitle :: WildeStyle
attributeTitle = WildeStyle [weAttributeClass,titleClass]

multiRow :: WildeStyle
multiRow = WildeStyle [weObjectClass]

pageTitleClasses,componentTitleClasses :: [ClassName]
pageTitleClasses      = [pageClass,titleClass]
componentTitleClasses = [componentClass,titleClass]

pageTopLevelComponentClasses :: [ClassName]
pageTopLevelComponentClasses = [pageClass, componentClass]
-- pageTopLevelComponentClasses = ["_develop__top_level_component"]

pageTitle,componentTitle :: WildeStyle
pageTitle      = WildeStyle pageTitleClasses
componentTitle = WildeStyle componentTitleClasses

actionsComponentClasses :: [ClassName]
actionsComponentClasses = [componentClass, actionsClass]

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
