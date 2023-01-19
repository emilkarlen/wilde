{-# LANGUAGE StrictData #-}

module Wilde.Driver.Application.WaiServer.Cli.Help.DocFormat
(
    Para(..),
    HdrValList(..),
    ExamplificationText(..),
    SectionContents(..),
    Section(..),

    parasSection,

    render,
    renderParas,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.List (intersperse)
import qualified Data.Char as Char

import           Options.Applicative.Help.Pretty


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Para
    = PText       String
    | PList       [String]
    | PHdrValList HdrValList
    | PLiteral    String
    | PExample    [Para]
    | PExamplification [ExamplificationText]

data ExamplificationText
    = ExText    String
    | ExLiteral String

data HdrValList
    = HvlCompact [(String, String)]
    | HvlParas   [(String, [Para])]

data SectionContents = SectionContents [Para] [Section]

data Section = Section String SectionContents

parasSection :: String -> [Para] -> Section
parasSection header paras = Section header $ SectionContents paras []

render :: SectionContents -> Doc
render = renderSC []

list_item_header :: String
list_item_header = "- "

fmt_list_item_header :: Doc -> Doc
fmt_list_item_header = (text list_item_header<>)

indent_example, indent_list, indent_dl_text :: Int
indent_example = 2
indent_list    = 2
indent_dl_text = 4

type ParentSect = [Int]
type SectNum    = Int

renderSC :: ParentSect -> SectionContents -> Doc
renderSC level (SectionContents [] subSections) = vcat $ renderSections level subSections
renderSC level (SectionContents initialParagraphs subSections) =
    vcat $ intersperse separator parts
    where
        separator :: Doc
        separator =
            if null level
                then empty <$$> empty
                else empty

        parts :: [Doc]
        parts = renderParas initialParagraphs : renderSections level subSections

renderParas :: [Para] -> Doc
renderParas = vcat . intersperse empty . map renderPara

renderPara :: Para -> Doc
renderPara (PText s)        = renderWords s
renderPara (PExamplification lines) = vcat formattedLines
    where
        formattedLines :: [Doc]
        formattedLines = map format lines

        format :: ExamplificationText -> Doc
        format (ExText    s) = renderWords s
        format (ExLiteral s) = indent 2 $ blue $ text s

renderPara (PList items)    = indent indent_list $ vcat $ map renderItem items
    where
        renderItem :: String -> Doc
        renderItem = fmt_list_item_header . indentLinesAfterItemMarker . renderWords

        indentLinesAfterItemMarker :: Doc -> Doc
        indentLinesAfterItemMarker = nest (length list_item_header)

renderPara (PHdrValList items) =
    case items of
        (HvlCompact items) -> renderItems id                  $ map rCompact items
        (HvlParas   items) -> renderItems (intersperse empty) $ map rParas   items
    where
        renderItems :: ([Doc] -> [Doc]) -> [(String, Doc)] -> Doc
        renderItems adjustItems = indent indent_list . vcat . adjustItems . map renderItem

        renderItem :: (String, Doc) -> Doc
        renderItem (concept, description) = nest indent_dl_text $ fmt_list_item_header (text concept)
                                            <$$> description

        rCompact :: (String, String) -> (String, Doc)
        rCompact (c,d) = (c, renderWords d)

        rParas :: (String, [Para]) -> (String, Doc)
        rParas (c,paras) = (c, linebreak <> renderParas paras)

renderPara (PLiteral s)     = vcat $ map (text . ("| "++)) $ lines s
renderPara (PExample paras) = indent indent_example $ renderParas $ PText "Example" : paras

renderSections :: ParentSect -> [Section] -> [Doc]
renderSections level sections = map render numbered_sections
    where
        render :: (SectNum, Section) -> Doc
        render (num, section) = renderSection (num, level) section

        numbered_sections :: [(SectNum, Section)]
        numbered_sections = zip (enumFrom 1) sections

renderSection :: (SectNum, ParentSect) -> Section -> Doc
renderSection level@(sNum, sParents) (Section header contents) =
    renderHeader level header <> linebreak <> linebreak <> renderSC (sNum : sParents) contents

renderHeader :: (SectNum, ParentSect) -> String -> Doc
renderHeader (sNum,sParents) s = header_style sParents $ text struct_str <+> text_style sParents s
    where
        struct_str :: String
        struct_str = concatMap ((<>".") . show) $ reverse $ sNum : sParents

        text_style :: ParentSect -> String -> Doc
        text_style []  = bold . text . upper
        text_style [_] = text . upper
        text_style _   = text

        header_style :: ParentSect -> Doc -> Doc
        header_style _   = bold

upper :: String -> String
upper = map Char.toUpper

renderWords :: String -- ^ a string with words separated by one or more spaces.
            -> Doc
renderWords s =
    case words s of
        []     -> empty
        [x]    -> text x
        (x:xs) -> foldl merge (text x) xs
    where
        merge :: Doc -> String -> Doc
        merge d s = d </> text s
