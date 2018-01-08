{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

-------------------------------------------------------------------------------
-- | Presentation for some common types.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.Presentation.Presentation
       (
         -- * Numbers
         
         pres_Word32,
         pres_Int32,
         
         presO_Integral,
         pres_Integral,
         presO_Integral_optional,
         pres_Integral_optional,
         
         presO_Fractional,
         pres_Fractional,
         presO_Fractional_optional,
         pres_Fractional_optional,
         
         -- * Date
         
         presO_Date,
         pres_Date,
         presO_Date_optional,
         pres_Date_optional,

         -- * String and Text
         
         presO_String,
         pres_String,
         presO_String_optional,
         pres_String_optional,
         
         presO_Text,
         pres_Text,
         presO_Text_optional,
         pres_Text_optional,
         
         presO_TextHtml,
         presO_TextHtml_optional,
         pres_TextHtml_optional,
         
         HtmlMultiLineTextValue(..),
         
         -- * Special

         presO_Href,
         pres_Href,
         presO_Href_optional,
         pres_Href_optional,
         
         -- * Utilities

         asUnquotedString,
         asUnquotedString_optional,

         showAsUnquotedString,
         showAsUnquotedString_optional,
         
         mkOptional,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Word
import Data.Int
import Data.Time.Calendar (Day)

import qualified Text.Html as Html

import Wilde.Media.WildeValue
import Wilde.WildeUi.StdValueTypes

import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------
         

pres_Word32 :: Title -- ^ Title of the attribute type.
            -> AttributeTypePresentation Word32
pres_Word32 = pres_Integral

pres_Int32 :: Title -- ^ Title of the attribute type.
           -> AttributeTypePresentation Int32
pres_Int32 = pres_Integral

pres_Integral :: Integral a
              => Title -- ^ Title of the attribute type.
              -> AttributeTypePresentation a
pres_Integral sTitle = AttributeTypePresentation
             {
               atpoOutput = presO_Integral
             , atpoTitle  = neutralTitle sTitle
             }

presO_Integral :: Integral a
               => PresentationOutputer a 
presO_Integral = AnySVALUE . IntValue . fromIntegral

pres_Integral_optional :: Integral a
                       => Title -- ^ Title of the attribute type.
                       -> AttributeTypePresentation (Maybe a)
pres_Integral_optional sTitle = AttributeTypePresentation
             {
               atpoOutput = presO_Integral_optional
             , atpoTitle  = neutralTitle sTitle
             }

presO_Integral_optional :: Integral a
                        => PresentationOutputer (Maybe a)
presO_Integral_optional = mkOptional presO_Integral

pres_Fractional :: (Fractional a,Show a)
                => Title -- ^ Title of the attribute type.
                -> AttributeTypePresentation a
pres_Fractional title = AttributeTypePresentation
             {
               atpoOutput = presO_Fractional
             , atpoTitle  = neutralTitle title
             }

presO_Fractional :: (Fractional a,Show a)
                 => PresentationOutputer a
presO_Fractional = showAsUnquotedString

pres_Fractional_optional :: (Fractional a,Show a)
                         => Title -- ^ Title of the attribute type.
                         -> AttributeTypePresentation (Maybe a)
pres_Fractional_optional title = AttributeTypePresentation
             {
               atpoOutput = presO_Fractional_optional
             , atpoTitle  = neutralTitle title
             }

presO_Fractional_optional :: (Fractional a,Show a)
                          => PresentationOutputer (Maybe a)
presO_Fractional_optional = mkOptional presO_Fractional


-------------------------------------------------------------------------------
-- - Date and Time -
-------------------------------------------------------------------------------


pres_Date :: Title -- ^ Title of the attribute type.
          -> AttributeTypePresentation Day
pres_Date sTitle = AttributeTypePresentation
             {
               atpoOutput = presO_Date
             , atpoTitle  = neutralTitle sTitle
             }

presO_Date :: PresentationOutputer Day
presO_Date = showAsUnquotedString

pres_Date_optional :: Title -- ^ Title of the attribute type.
                   -> AttributeTypePresentation (Maybe Day)
pres_Date_optional sTitle = AttributeTypePresentation
             {
               atpoOutput = presO_Date_optional
             , atpoTitle  = neutralTitle sTitle
             }

presO_Date_optional :: PresentationOutputer (Maybe Day)
presO_Date_optional = mkOptional presO_Date


-------------------------------------------------------------------------------
-- - String and Text -
-------------------------------------------------------------------------------


pres_String :: String 
            -> AttributeTypePresentation String
pres_String sTitle = AttributeTypePresentation
                    {
                      atpoOutput = presO_String
                    , atpoTitle  = neutralTitle sTitle
                    }

presO_String :: PresentationOutputer String
presO_String = AnySVALUE . UnquotedStringValue

pres_String_optional :: String 
                     -> AttributeTypePresentation (Maybe String)
pres_String_optional sTitle = AttributeTypePresentation
                    {
                      atpoOutput = presO_String_optional
                    , atpoTitle  = neutralTitle sTitle
                    }

presO_String_optional :: PresentationOutputer (Maybe String)
presO_String_optional = mkOptional presO_String

pres_Text :: String 
          -> AttributeTypePresentation String
pres_Text sTitle = AttributeTypePresentation
                    {
                      atpoOutput = presO_Text
                    , atpoTitle  = neutralTitle sTitle
                    }

presO_Text :: PresentationOutputer String
presO_Text = AnySVALUE . UnquotedMultiLineTextValue

pres_Text_optional :: String 
                   -> AttributeTypePresentation (Maybe String)
pres_Text_optional sTitle = AttributeTypePresentation
                    {
                      atpoOutput = maybe empty (AnySVALUE . UnquotedMultiLineTextValue),
                      atpoTitle  = neutralTitle sTitle
                    }

presO_Text_optional :: PresentationOutputer (Maybe String)
presO_Text_optional = mkOptional presO_Text

presO_TextHtml :: PresentationOutputer String
presO_TextHtml = AnySVALUE . HtmlMultiLineTextValue

pres_TextHtml_optional :: String 
                       -> AttributeTypePresentation (Maybe String)
pres_TextHtml_optional sTitle = AttributeTypePresentation
                    {
                      atpoOutput = presO_TextHtml_optional
                    , atpoTitle  = neutralTitle sTitle
                    }

presO_TextHtml_optional :: PresentationOutputer (Maybe String)
presO_TextHtml_optional = mkOptional presO_TextHtml

-- | A String that is HTML formated.
newtype HtmlMultiLineTextValue = HtmlMultiLineTextValue String

instance VALUE HtmlMultiLineTextValue where
  valueString (HtmlMultiLineTextValue x) = x
  valueHtml   (HtmlMultiLineTextValue x) = Html.primHtml x


instance SVALUE HtmlMultiLineTextValue



-------------------------------------------------------------------------------
-- - HREF -
-------------------------------------------------------------------------------


pres_Href :: String 
          -> AttributeTypePresentation String
pres_Href sTitle = AttributeTypePresentation
                    {
                      atpoOutput = presO_Href
                    , atpoTitle  = neutralTitle sTitle
                    }

presO_Href :: PresentationOutputer String
presO_Href = AnySVALUE . HrefValue . (\x -> (x,x))

pres_Href_optional :: String 
                   -> AttributeTypePresentation (Maybe String)
pres_Href_optional sTitle = AttributeTypePresentation
                    {
                      atpoOutput = presO_Href_optional
                    , atpoTitle  = neutralTitle sTitle
                    }

presO_Href_optional :: PresentationOutputer (Maybe String)
presO_Href_optional = mkOptional presO_Href


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


showAsUnquotedString :: Show a => PresentationOutputer a
showAsUnquotedString = asUnquotedString show

asUnquotedString :: (a -> String) -> PresentationOutputer a
asUnquotedString f = AnySVALUE . UnquotedStringValue . f

mkOptional :: PresentationOutputer a
           -> PresentationOutputer (Maybe a)
mkOptional = maybe empty

showAsUnquotedString_optional :: Show a => PresentationOutputer (Maybe a)
showAsUnquotedString_optional = maybe empty showAsUnquotedString

asUnquotedString_optional :: (a -> String) -> PresentationOutputer (Maybe a)
asUnquotedString_optional f = maybe empty (asUnquotedString f)
