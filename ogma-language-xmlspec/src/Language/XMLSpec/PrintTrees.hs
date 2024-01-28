{-# LANGUAGE FlexibleInstances #-}
-- Copyright 2024 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
-- OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
-- LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
-- SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
-- SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
-- PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
-- ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
-- RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
-- ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
-- GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
-- THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
-- IT "AS IS."
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
-- THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
-- ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
-- ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
-- INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
-- USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
-- UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
-- PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
-- FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
-- AGREEMENT.

-- | Print XML trees.
--
-- This variant of the function to show XML trees available in HXT is needed
-- because of issues with values being quoted by HXT's default function.
--
-- It's based on the same ideas, but it's been implemented using the
-- pretty-library instead of using plain string functions.
module Language.XMLSpec.PrintTrees where

-- External imports
import Data.Maybe                   (fromMaybe, isNothing)
import Data.Tree.NTree.TypeDefs     (NTree (NTree))
import Prelude                      hiding (quot, (<>))
import Text.PrettyPrint.HughesPJ    (Doc, Mode (..), Style (..), brackets, char,
                                     colon, doubleQuotes, empty, equals, hcat,
                                     int, isEmpty, parens, renderStyle, space,
                                     text, vcat, (<+>), (<>))
import Text.Regex.XMLSchema.Generic (sed)
import Text.XML.HXT.Core            hiding (getDTDAttrl, getNode, mkDTDElem,
                                     xshow, (<+>), txt)
import Text.XML.HXT.DOM.ShowXml     (xshow)
import Text.XML.HXT.DOM.XmlNode     (getDTDAttrl, getNode, mkDTDElem)

-- | Render a document into a string.
flattenDoc :: Doc -> String
flattenDoc = renderStyle (Style LeftMode 0 0)

-- | Class for values that can be converted into a document.
class Pretty x where
  pretty :: x -> Doc

instance Pretty [XmlTree] where
  pretty = hcat . map pretty

instance Pretty XmlTree where
  pretty (NTree (XText s) _) =
      text (textEscapeXml' s)
    where
      -- | Auxiliary function to escape certain XML characters
      textEscapeXml' :: String -> String
      textEscapeXml' = concatMap textEscapeChar
        where
          textEscapeChar '<' = "&lt;"
          textEscapeChar '>' = "&gt;"
          textEscapeChar '&' = "&amp;"
          textEscapeChar x   = [x]

  pretty (NTree (XBlob blob) _) =
    text (blobToString blob)

  pretty (NTree (XCharRef ref) _) =
    text "&#" <> int ref <> char ';'

  pretty (NTree (XEntityRef ref) _) =
    text "&" <> text ref <> char ';'

  pretty (NTree (XCmt comment) _) =
    text "<!--" <> text comment <> text "-->"

  pretty (NTree (XCdata txt) _) =
      text "<![CDATA[" <> text txt' <> text "]]>"
    where
      -- Escape "]]>" if present in the data contents
      txt' = sed (const "]]&gt;") "\\]\\]>" txt

  pretty (NTree (XPi iName attributes) _) =
         text "<?"
      <> pretty iName
      <> hcat (map prettyPIAttr attributes)
      <> text "?>"
    where
      -- Print an attribute of a processing instruction.
      prettyPIAttr :: XmlTree -> Doc
      prettyPIAttr attrs
        | (NTree (XAttr attrQName) children) <- attrs
        , qualifiedName attrQName == a_value
        = space <> pretty children

        -- <?xml version="..." ... ?>
        | otherwise
        = pretty attrs

  pretty (NTree (XTag tagQName attributeList) []) =
    angles (pretty tagQName <> pretty attributeList <> slash)

  pretty (NTree (XTag tagQName attributeList) children) =
       angles (pretty tagQName <> pretty attributeList) <> pretty children
    <> angles (slash <> pretty tagQName)

  pretty (NTree (XDTD dtdElem attributeList) children) =
    pretty (dtdElem, attributeList, children)

  pretty (NTree (XAttr attrQName) children) =
    space <> pretty attrQName <> equals <> doubleQuotes (pretty children)

  pretty (NTree (XError level txt) _) =
        text "<!-- ERROR" <+> parens (int level) <> colon
    <|> text txt <|> text "-->"

instance Pretty (DTDElem, Attributes, XmlTrees) where
  pretty (DOCTYPE, attributeList, children) =
          text "<!DOCTYPE "
      <>  pretty (a_name, attributeList)
      <>  prettyExternalId attributeList
      <+> prettyInternalDTD children
      <>  text ">"
    where
      prettyInternalDTD [] = empty
      prettyInternalDTD ds = brackets $ nl <> vcat (map pretty ds)

  pretty (ELEMENT, attributeList, children) =
        text "<!ELEMENT "
    <>  pretty (a_name, attributeList)
    <+> prettyElemType (lookup1 a_type attributeList) children
    <>  text " >"

  pretty (CONTENT, attributeList, children) =
    prettyContent (mkDTDElem CONTENT attributeList children)

  pretty (ATTLIST, attributeList, children) =
         text "<!ATTLIST "
      <> ( if isNothing (lookup a_name attributeList)
            then pretty children
            else pretty (a_name, attributeList)
                   <+> prettyValue attributeList children
         )
      <> text " >"

  pretty (ENTITY, attributeList, children) =
    prettyEntity "" attributeList children

  pretty (PENTITY, attributeList, children) =
    prettyEntity "% " attributeList children

  pretty (NOTATION, attributeList, _children) =
       text "<!NOTATION "
    <> pretty (a_name, attributeList)
    <> prettyExternalId attributeList
    <> text " >"

  pretty (CONDSECT, _, child:children) =
       text "<![ "
    <> pretty child
    <> text " [\n"
    <> pretty children
    <> text "]]>"

  pretty (CONDSECT, _, []) =
    empty

  pretty (NAME, attributeList, _children) =
    pretty (a_name, attributeList)

  pretty (PEREF, attributeList, _children) =
    prettyPEAttr attributeList

instance Pretty QName where
  pretty = text . qualifiedName

instance Pretty (String, Attributes) where
  pretty (k, attributeList) = text (lookup1 k attributeList)

-- * Auxiliary functions related to pretty printing XML trees.

-- | Pretty print an attribute followed by its value.
prettyAttr :: String -> Attributes -> Doc
prettyAttr k attributeList
  | Just v <- lookup k attributeList
  = text k <+> text v
  | otherwise
  = empty

-- | Pretty print a content element.
prettyContent :: XmlTree -> Doc
prettyContent (NTree (XDTD NAME attributeList) _) =
  pretty (a_name, attributeList)
prettyContent (NTree (XDTD PEREF attributeList) _) =
  prettyPEAttr attributeList
prettyContent (NTree (XDTD CONTENT attributeList) children) =
       parens (sepBy separator (map prettyContent children))
    <> pretty (a_modifier, attributeList)
  where
    separator = text (if a_kind == v_seq then ", " else " | ")
prettyContent (NTree (XDTD n _) _) =
  error $ "prettyContent " ++ show n ++ " is undefined"
prettyContent tree = pretty tree

-- | Pretty print the type of an element.
prettyElemType :: String -> XmlTrees -> Doc
prettyElemType elemType children
    | elemType == v_pcdata
    = parens (text v_pcdata)

    | elemType == v_mixed && not (null children)
    , let [NTree (XDTD CONTENT attributeList') children'] = children
    = parens
        ( sepBy
            (text " | ")
            ( text v_pcdata
            : map (prettyEnum . treeElemAttributes. getNode) children'
            )
        )
    <> pretty (a_modifier, attributeList')

    | elemType == v_mixed -- incorrect tree
    = parens empty

    | elemType == v_children && not (null children)
    = prettyContent (head children)

    | elemType == v_children
    = parens empty

    | elemType == k_peref
    = hcat $ map prettyContent children

    | otherwise
    = text elemType

  where
    treeElemAttributes (XDTD _ attributeList') = attributeList'
    treeElemAttributes (XText txt) = [(a_name, txt)]
    treeElemAttributes _           = []

-- | Pretty print an entity.
prettyEntity :: String -> Attributes -> XmlTrees -> Doc
prettyEntity kind attributeList children =
      text "<!ENTITY "
  <>  text kind
  <>  pretty (a_name, attributeList)
  <>  prettyExternalId attributeList
  <+> prettyAttr k_ndata attributeList
  <+> prettyLiteralTrees children
  <>  text " >"

-- | Pretty print trees as text, quoting them.
prettyLiteralTrees :: XmlTrees -> Doc
prettyLiteralTrees []       = empty
prettyLiteralTrees children = doubleQuotes $ text $ xshow children

-- | Pretty print an external ID.
prettyExternalId :: Attributes -> Doc
prettyExternalId attributeList =
  case (lookup k_system attributeList, lookup k_public attributeList) of
    (Nothing, Nothing) -> empty
    (Just s,  Nothing) -> text k_system <+> doubleQuotes (text s)
    (Nothing, Just p ) -> space <> text k_public <+> doubleQuotes (text p)
    (Just s,  Just p ) -> space <> text k_public <+> doubleQuotes (text p)
                            <+> doubleQuotes (text s)

-- | Pretty print a Parameter Entity Reference.
prettyPEAttr :: Attributes -> Doc
prettyPEAttr = maybe empty (\pe -> char '%' <> text pe <> char ';')
             . lookup a_peref

-- | Given a list of attributes, pretty print the value in them.
prettyValue :: Attributes -> XmlTrees -> Doc
prettyValue attributeList children
    | Just aValue <- lookup a_value attributeList
    =     text aValue
      <+> prettyAttrType (lookup1 a_type attributeList)
      <+> prettyAttrKind (lookup1 a_kind attributeList)

    | otherwise
    = prettyPEAttr $ fromMaybe [] $ getDTDAttrl $ head children
  where

    prettyAttrType attrType
      | attrType == k_peref       = prettyPEAttr attributeList
      | attrType == k_enumeration = prettyAttrEnum
      | attrType == k_notation    = text k_notation <+> prettyAttrEnum
      | otherwise                 = text attrType

    prettyAttrEnum =
        parens $ sepBy (text " | ") $
          map (prettyEnum . fromMaybe [] . getDTDAttrl) children
      where

    prettyAttrKind kind
      | kind == k_default
      = doubleQuotes (text (lookup1 a_default attributeList))

      | kind == k_fixed
      = text k_fixed
        <+> doubleQuotes (text (lookup1 a_default attributeList))

      | otherwise
      = text kind

-- Pretty print the name of an attribute, followed by the PE Reference.
prettyEnum :: Attributes -> Doc
prettyEnum attributes = pretty (a_name, attributes) <> prettyPEAttr attributes

-- ** Generic document constructors

-- | Forward slash character.
slash :: Doc
slash = char '/'

-- | New line character.
nl :: Doc
nl = char '\n'

-- | Enclose document in angle brackets.
angles :: Doc -> Doc
angles s = char '<' <> s <> char '>'

-- | Compose two documents, separating them by a new line.
--
-- The new line is not inserted if either document is empty.
(<|>) :: Doc -> Doc -> Doc
(<|>) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> nl <> y

-- | Concatenate a list od documents, separating them by a given separator.
sepBy :: Doc   -- ^ Separator
      -> [Doc] -- ^ List of documents
      -> Doc
sepBy _   []     = empty
sepBy _   [x]    = x
sepBy sep (x:xs) = x <> sep <> sepBy sep xs
