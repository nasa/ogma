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

-- | Parser for Ogma specs stored in XML files.
module Language.XMLSpec.Parser where

-- External imports
import Prelude hiding (showChar, showString)
import Control.Monad            (when)
import Control.Monad.Except     (ExceptT (..), liftEither, throwError)
import Control.Monad.IO.Class   (liftIO)
import Data.List                (isPrefixOf, isInfixOf)
import Data.Maybe               (fromMaybe, isNothing)
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.XmlNode (getDTDAttrl, mkDTDElem, getNode)
import Text.XML.HXT.Core hiding (xshow, getDTDAttrl, mkDTDElem, getNode)
--         (listA, configSysVars, no, readString, runX,
--                                  withOutputPLAIN, withSubstDTDEntities, withErrors, withValidate, withRedirect, withCanonicalize, withRemoveWS, yes, (>>>),  xshowEscapeXml, Attributes)
import Text.XML.HXT.DOM.ShowXml (xshow)
import Text.XML.HXT.XPath       (getXPathTrees, parseXPathExpr)
import Text.Pretty.Simple
import Text.Regex.XMLSchema.Generic (sed)
import Text.XML.HXT.DOM.Util (textEscapeXml)

-- External imports: ogma-spec
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (Spec))

-- | List of XPath routes to the elements we need to parse.
--
-- The optional paths denote elements that may not exist. If they are nothing,
-- those elements are not parsed in the input file.
--
-- The subfields are applied on each string matching the parent element. That
-- is, the internal var ID XPath will be a applied to the strings returned when
-- applying the internal vars XPath (if it exists). Paths whose names are
-- plural denote expected lists of elements.
--
-- The components of a tuple (String, Maybe (String, String)) mean the
-- following: if a string is present but the second component is Nothing, then
-- the string is the XPath expression to be used. If a Just value is present,
-- the first element of its inner tuple represents a key, and the second
-- element represents an XPath expression that will produce a value when
-- evaluated globally in the file. After evaluating that expression, the key
-- must be found in the first string of the three and replaced with the result
-- of evaluating the expression.
data XMLFormat = XMLFormat
  { specInternalVars    :: Maybe String
  , specInternalVarId   :: (String, Maybe (String, String))
  , specInternalVarExpr :: (String, Maybe (String, String))
  , specInternalVarType :: Maybe (String, Maybe (String, String))
  , specExternalVars    :: Maybe String
  , specExternalVarId   :: (String, Maybe (String, String))
  , specExternalVarType :: Maybe (String, Maybe (String, String))
  , specRequirements    :: (String, Maybe (String, String))
  , specRequirementId   :: (String, Maybe (String, String))
  , specRequirementDesc :: Maybe (String, Maybe (String, String))
  , specRequirementExpr :: (String, Maybe (String, String))
  }

-- | Parse an XML file and extract a Spec from it.
--
-- An auxiliary function must be provided to parse the requirement expressions.
--
-- Fails if any of the XPaths in the argument XMLFormat are not valid
-- expressions, of the XML is malformed, or if the elements are not found with
-- the frequency expected (e.g., an external variable id is not found even
-- though external variables are found).
parseXMLSpec :: (String -> ExceptT String IO a) -- ^ Parser for expressions.
             -> a
             -> XMLFormat                       -- ^ XPaths for spec locations.
             -> String                          -- ^ String containing XML
             -> ExceptT String IO (Spec a)
parseXMLSpec parseExpr defA xmlFormat value = do
  xmlFormatInternal <- parseXMLFormat xmlFormat value

  -- Internal variables

  -- let intVarStrings :: [String]
  intVarStrings <- liftIO $ maybe
                              (return [])
                              (`executeXPath` value)
                              (xfiInternalVars xmlFormatInternal)

  let internalVarDef :: String -> ExceptT String IO InternalVariableDef
      internalVarDef def = do
        let msgI = "internal variable name"
        varId <- ExceptT $
                   listToEither msgI <$>
                   executeXPath (xfiInternalVarId xmlFormatInternal) def

        let msgT = "internal variable type"
        varType <- maybe
                     (liftEither $ Right "")
                     (\e -> ExceptT $ listToEither msgT <$> executeXPath e def)
                     (xfiInternalVarType xmlFormatInternal)

        let msgE = "internal variable expr"
        varExpr <- ExceptT $
                     listToEither msgE <$>
                     executeXPath (xfiInternalVarExpr xmlFormatInternal) def

        return $ InternalVariableDef
                   { internalVariableName = varId
                   , internalVariableType = varType
                   , internalVariableExpr = varExpr
                   }

  internalVariableDefs <- mapM internalVarDef intVarStrings

  -- External variables

  -- let extVarStrings :: [String]
  extVarStrings <- liftIO $ maybe
                              (return [])
                              (`executeXPath` value)
                              (xfiExternalVars xmlFormatInternal)

  let externalVarDef :: String -> ExceptT String IO ExternalVariableDef
      externalVarDef def = do

        let msgI = "external variable name"
        varId <- ExceptT $
                   listToEither msgI <$>
                   executeXPath (xfiExternalVarId xmlFormatInternal) def

        let msgT = "external variable type"
        varType <- maybe
                     (liftEither $ Right "")
                     (\e -> ExceptT $ listToEither msgT <$> executeXPath e def)
                     (xfiExternalVarType xmlFormatInternal)

        return $ ExternalVariableDef
                   { externalVariableName = varId
                   , externalVariableType = varType
                   }

  externalVariableDefs <- mapM externalVarDef extVarStrings

  -- Requirements

  -- let reqStrings :: [String]
  reqStrings <- liftIO $ executeXPath (xfiRequirements xmlFormatInternal) value

  let -- requirementDef :: String -> ExceptT String (Requirement a)
      requirementDef def = do
        let msgI = "Requirement name: " ++ take 160 def
        reqId <- liftIO $ fromMaybe "" . listToMaybe <$>
                   executeXPath (xfiRequirementId xmlFormatInternal) def

        -- liftIO $ pPrint (xfiRequirementExpr xmlFormatInternal)
        let msgE = "Requirement expression: " ++ take 160 def
        reqExpr <- liftIO $
                     listToMaybe <$>
                     (concatMapM (`executeXPath` def) (xfiRequirementExpr xmlFormatInternal))

        reqExpr' <- maybe (return defA) parseExpr reqExpr

        let msgD = "Requirement description"
        reqDesc <- maybe
                     (liftEither $ Right "")
                     (\e -> liftIO $ (fromMaybe "" . listToMaybe) <$> executeXPath e def)
                     (xfiRequirementDesc xmlFormatInternal)

        -- liftIO $ print (xfiRequirementExpr xmlFormatInternal)
        -- liftIO $ putStrLn reqId
        -- liftIO $ putStrLn reqExpr

        return $ Requirement
                   { requirementName        = reqId
                   , requirementExpr        = reqExpr'
                   , requirementDescription = reqDesc
                   }

  requirements <- mapM requirementDef reqStrings

  -- Complete spec

  return $ Spec internalVariableDefs externalVariableDefs requirements

-- | Internal representation of an XML Format specification.
data XMLFormatInternal = XMLFormatInternal
  { xfiInternalVars    :: Maybe XPathExpr
  , xfiInternalVarId   :: XPathExpr
  , xfiInternalVarExpr :: XPathExpr
  , xfiInternalVarType :: Maybe XPathExpr
  , xfiExternalVars    :: Maybe XPathExpr
  , xfiExternalVarId   :: XPathExpr
  , xfiExternalVarType :: Maybe XPathExpr
  , xfiRequirements    :: XPathExpr
  , xfiRequirementId   :: XPathExpr
  , xfiRequirementDesc :: Maybe XPathExpr
  , xfiRequirementExpr :: [XPathExpr]
  }

-- | Internal representation of an XPath expression.
type XPathExpr = String

resolveIndirectly :: String -> (String, Maybe (String, String)) -> ExceptT String IO XPathExpr
resolveIndirectly _   (query, Nothing)         = liftEither $ checkXPathExpr query
resolveIndirectly xml (query, Just (key, val)) = do
  -- Check that the given query string parses correctly.
  _ <- liftEither $ checkXPathExpr val

  v  <- liftIO $ executeXPath val xml
  case v of
    (f:_) -> do let query' = replace query key f
                liftEither $ checkXPathExpr query'
    _     -> throwError $ "Substitution path " ++ show val ++ " not found in file."

resolveIndirectly' :: String -> (String, Maybe (String, String)) -> ExceptT String IO [XPathExpr]
resolveIndirectly' _   (query, Nothing)         = fmap (:[]) $ liftEither $ checkXPathExpr query
resolveIndirectly' xml (query, Just (key, val)) = do
  -- Check that the given query string parses correctly.
  _ <- liftEither $ checkXPathExpr val

  v  <- liftIO $ executeXPath val xml
  case v of
    [] -> throwError $ "Substitution path " ++ show val ++ " not found in file."
    fs -> do let queries = map (replace query key) fs
             liftEither $ mapM checkXPathExpr queries

replace :: String -> String -> String -> String
replace []           _k  _v    = []
replace string@(h:t) key value
  | key `isPrefixOf` string
  = value ++ replace (drop (length key) string) key value
  | otherwise
  = h : replace t key value

checkXPathExpr :: String -> Either String XPathExpr
checkXPathExpr s = s <$ parseXPathExpr s

-- | Check an XMLFormat and return an internal representation.
--
-- Fails with an error message if any of the given expressions are not a valid
-- XPath expression.
parseXMLFormat :: XMLFormat -> String -> ExceptT String IO XMLFormatInternal
parseXMLFormat xmlFormat file = do
  xfi2  <- liftEither $ swapMaybeEither $ checkXPathExpr <$> specInternalVars xmlFormat
  xfi3  <- resolveIndirectly file $ specInternalVarId xmlFormat
  xfi4  <- resolveIndirectly file $ specInternalVarExpr xmlFormat
  xfi5  <- swapMaybeExceptT $ resolveIndirectly file <$> specInternalVarType xmlFormat
  xfi6  <- liftEither $ swapMaybeEither $ checkXPathExpr <$> specExternalVars xmlFormat
  xfi7  <- resolveIndirectly file $ specExternalVarId xmlFormat
  xfi8  <- swapMaybeExceptT $ resolveIndirectly file <$> specExternalVarType xmlFormat
  xfi9  <- resolveIndirectly file $ specRequirements xmlFormat
  xfi10 <- resolveIndirectly file $ specRequirementId xmlFormat
  xfi11 <- swapMaybeExceptT $ resolveIndirectly file <$> specRequirementDesc xmlFormat
  xfi12 <- resolveIndirectly' file $ specRequirementExpr xmlFormat
  return $ XMLFormatInternal
             { xfiInternalVars    = xfi2
             , xfiInternalVarId   = xfi3
             , xfiInternalVarExpr = xfi4
             , xfiInternalVarType = xfi5
             , xfiExternalVars    = xfi6
             , xfiExternalVarId   = xfi7
             , xfiExternalVarType = xfi8
             , xfiRequirements    = xfi9
             , xfiRequirementId   = xfi10
             , xfiRequirementDesc = xfi11
             , xfiRequirementExpr = xfi12
             }

-- | Execute an XPath query in an XML string, returning the list of strings
-- that match the path.
executeXPath :: String -> String -> IO [String]
executeXPath query string = do
  let config = [withValidate no, withRedirect no, withCanonicalize no, withRemoveWS yes, withSubstDTDEntities no, withOutputPLAIN]
  v <- runX $ configSysVars config >>> (readString config string >>> getXPathTrees query)

  -- We apply xshow to every tree to turn it back into a string. That will
  -- allow us to use the same function to select subparts of the matched
  -- expression.
  -- return $ map (xshow . (:[])) v
  -- putStrLn "List starts here ***************************************"
  -- mapM_ pPrint v
  -- putStrLn "List ends here ***************************************"
  -- putStrLn "List ends here ***************************************"
  -- putStrLn "List ends here ***************************************"
  return $ map ((\x -> showXmlTrees showString showString x "") . (:[])) v

f query string = do
  let config = [withValidate no, withRedirect no, withCanonicalize no, withRemoveWS yes, withSubstDTDEntities no, withTextMode yes]
  v <- runX $ configSysVars config >>> (readString config string >>> getXPathTrees query)
  return v

-- * Auxiliary

-- | Swap the Maybe and Either layers of a value.
swapMaybeEither :: Maybe (Either a b) -> Either a (Maybe b)
swapMaybeEither Nothing          = Right Nothing
swapMaybeEither (Just (Left s))  = Left s
swapMaybeEither (Just (Right x)) = Right $ Just x

-- | Swap the Maybe and Either layers of a value.
swapMaybeExceptT :: Monad m => Maybe (ExceptT a m b) -> ExceptT a m (Maybe b)
swapMaybeExceptT Nothing   = return Nothing
swapMaybeExceptT (Just e)  = Just <$> e

-- | Convert a list to an Either, failing if the list provided does not have
-- exactly one value.
listToEither :: String -> [String] -> Either String String
listToEither _   [x] = Right x
listToEither msg []  = Left $ "Failed to find a value for " ++ msg
listToEither msg _   = Left $ "Unexpectedly found multiple values for " ++ msg

listToMaybe :: [String] -> Maybe String
listToMaybe (x:_) = Just x
listToMaybe []    = Nothing

reqIfFormat :: XMLFormat
reqIfFormat = XMLFormat
    { specInternalVars    = Nothing
    , specInternalVarId   = ("//*", Nothing)
    , specInternalVarExpr = ("//*", Nothing)
    , specInternalVarType = Nothing
    , specExternalVars    = Nothing
    , specExternalVarId   = ("//*", Nothing)
    , specExternalVarType = Nothing
    , specRequirements    = (requirements, Just ("KEY", requirementsKey))
    , specRequirementId   = (requirementId, Nothing)
    , specRequirementDesc = Just (requirementNameVal, Just ("KEY", requirementNameKey))
    , specRequirementExpr = (requirementExprVal, Just ("KEY", requirementExprKey))
    }
  where
    -- requirementExprVal = "//ATTRIBUTE-VALUE-XHTML/DEFINITION/ATTRIBUTE-DEFINITION-XHTML-REF[contains(text(),'KEY')]/../../THE-VALUE/xhtml:div/text()"
    -- requirementExprVal = "//ATTRIBUTE-VALUE-XHTML/DEFINITION/ATTRIBUTE-DEFINITION-XHTML-REF[contains(text(),'KEY')]/../../THE-VALUE/div/text()"
    requirementExprVal = "//ATTRIBUTE-VALUE-XHTML/DEFINITION/ATTRIBUTE-DEFINITION-XHTML-REF[contains(text(),'KEY')]/../../THE-VALUE/div/*"

    requirementExprKey = "//ATTRIBUTE-DEFINITION-XHTML[contains(@LONG-NAME, \"ReqIF.Text\")]/@IDENTIFIER/text()"

    requirementNameVal = "//ATTRIBUTE-VALUE-XHTML/DEFINITION/ATTRIBUTE-DEFINITION-XHTML-REF[contains(text(),'KEY')]/../../THE-VALUE/div/*"
    requirementNameKey = "//ATTRIBUTE-DEFINITION-XHTML[contains(@LONG-NAME, \"ReqIF.Name\")]/@IDENTIFIER/text()"

    requirements = "//SPEC-OBJECTS/SPEC-OBJECT/TYPE/SPEC-OBJECT-TYPE-REF[contains(text(),\"KEY\")]/../.."
    requirementsKey = "//SPEC-OBJECT-TYPE[contains(@LONG-NAME, \"Requirement\")]/@IDENTIFIER/text()"

    requirementId = "//SPEC-OBJECT/@IDENTIFIER/text()"

-- TEMPORARY. DO NOT PUBLISH.
-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; pure $ x++xs

mdFormat :: XMLFormat
mdFormat = XMLFormat
    { specInternalVars    = Nothing
    , specInternalVarId   = ("//*", Nothing)
    , specInternalVarExpr = ("//*", Nothing)
    , specInternalVarType = Nothing
    , specExternalVars    = Nothing
    , specExternalVarId   = ("//*", Nothing)
    , specExternalVarType = Nothing
    , specRequirements    = (requirements, Nothing)
    , specRequirementId   = (requirementId, Nothing)
    , specRequirementDesc = Just (requirementNameVal, Nothing)
    , specRequirementExpr = (requirementExprVal, Nothing)
    }
  where
    requirementExprVal = "//sysml:Requirement/@Text/text()"

    requirementNameVal = "//sysml:Requirement/@Id/text()"

    requirements = "//sysml:Requirement"
    requirementId = "//sysml:Requirement/@Id/text()"

-- ------------------------------------------------------------

-- The following code is copied and adapted from HXT and cannot be included in
-- the final distribution of Ogma.
--
-- It is included here because I need to adapt it, but I think the license
-- might not allow us to redistribute it under NOSA or as part of Ogma.
--
-- There may be a way to implement a similar feature without having to
-- replicate this code here; I probably just don't know enough about HXT.

type StringFct          = String -> String

-- ------------------------------------------------------------

showXmlTrees                    :: (String -> StringFct) ->
                                   (String -> StringFct) ->
                                   XmlTrees -> StringFct
showXmlTrees cf af
    = showTrees
      where

      -- ------------------------------------------------------------

      showTrees                 :: XmlTrees -> StringFct
      showTrees                 = foldr (.) id . map showXmlTree
      {-# INLINE showTrees #-}

      showTrees'                :: XmlTrees -> StringFct
      showTrees'                = foldr (\ x y -> x . showNL . y) id . map showXmlTree
      {-# INLINE showTrees' #-}

      -- ------------------------------------------------------------

      showXmlTree             :: XmlTree  -> StringFct
      showXmlTree (NTree (XText s) _)                         -- common cases first
                                = cf (textEscapeXml' s)

      showXmlTree (NTree (XTag t al) [])
                                = showLt . showQName t . showTrees al . showSlash . showGt

      showXmlTree (NTree (XTag t al) cs)
                                = showLt . showQName t . showTrees al . showGt
                                  . showTrees cs
                                  . showLt . showSlash . showQName t . showGt

      showXmlTree (NTree (XAttr an) cs)
                                = showBlank
                                  . showQName an
                                  . showEq
                                  . showQuot
                                  . af (xshow cs)
                                  . showQuot

      showXmlTree (NTree (XBlob b) _)
                                = cf . blobToString $ b

      showXmlTree (NTree (XCharRef i) _)
                                = showString "&#" . showString (show i) . showChar ';'

      showXmlTree (NTree (XEntityRef r) _)
                                = showString "&" . showString r . showChar ';'

      showXmlTree (NTree (XCmt c) _)
                                = showString "<!--" . showString c . showString "-->"

      showXmlTree (NTree (XCdata d) _)
                                = showString "<![CDATA[" . showString d' . showString "]]>"
                                  where
                                    -- quote "]]>" in CDATA contents
                                    d' = sed (const "]]&gt;") "\\]\\]>" d

      showXmlTree (NTree (XPi n al) _)
                                = showString "<?"
                                  . showQName n
                                  . (foldr (.) id . map showPiAttr) al
                                  . showString "?>"
                                  where
                                  showPiAttr        :: XmlTree -> StringFct
                                  showPiAttr a@(NTree (XAttr an) cs)
                                      | qualifiedName an == a_value
                                          -- <?some-pi ... ?>
                                          -- no XML quoting of PI value
                                          = showBlank . showXmlTrees showString showString cs
                                      | otherwise
                                          -- <?xml version="..." ... ?>
                                          = showXmlTree a
                                  showPiAttr a
                                      = showXmlTree a -- id

      showXmlTree (NTree (XDTD de al) cs)
                                = showXmlDTD de al cs

      showXmlTree (NTree (XError l e) _)
                                = showString "<!-- ERROR ("
                                  . shows l
                                  . showString "):\n"
                                  . showString e
                                  . showString "\n-->"

      -- ------------------------------------------------------------

      showXmlDTD              :: DTDElem -> Attributes -> XmlTrees -> StringFct

      showXmlDTD DOCTYPE al cs  = showString "<!DOCTYPE "
                                  . showAttr a_name al
                                  . showExternalId al
                                  . showInternalDTD cs
                                  . showString ">"
                                  where
                                  showInternalDTD [] = id
                                  showInternalDTD ds = showString " [\n"
                                                       . showTrees' ds
                                                       . showChar ']'

      showXmlDTD ELEMENT al cs  = showString "<!ELEMENT "
                                  . showAttr a_name al
                                  . showBlank
                                  . showElemType (lookup1 a_type al) cs
                                  . showString " >"

      showXmlDTD ATTLIST al cs  = showString "<!ATTLIST "
                                  . ( if isNothing . lookup a_name $ al
                                      then
                                      showTrees cs
                                      else
                                      showAttr a_name al
                                      . showBlank
                                      . ( case lookup a_value al of
                                          Nothing -> ( showPEAttr
                                                       . fromMaybe [] . getDTDAttrl
                                                       . head
                                                     ) cs
                                          Just a  -> ( showString a
                                                       . showAttrType (lookup1 a_type al)
                                                       . showAttrKind (lookup1 a_kind al)
                                                     )
                                        )
                                    )
                                  . showString " >"
                                  where
                                  showAttrType t
                                      | t == k_peref
                                          = showBlank . showPEAttr al
                                      | t == k_enumeration
                                          = showAttrEnum
                                      | t == k_notation
                                          = showBlank . showString k_notation . showAttrEnum
                                      | otherwise
                                          = showBlank . showString t

                                  showAttrEnum
                                      = showString " ("
                                        . foldr1
                                              (\ s1 s2 -> s1 . showString " | " .  s2)
                                              (map (getEnum . fromMaybe [] . getDTDAttrl) cs)
                                        . showString ")"
                                        where
                                        getEnum     :: Attributes -> StringFct
                                        getEnum l = showAttr a_name l . showPEAttr l

                                  showAttrKind k
                                      | k == k_default
                                          = showBlank
                                            . showQuoteString (lookup1 a_default al)
                                      | k == k_fixed
                                          = showBlank
                                            . showString k_fixed
                                            . showBlank
                                            . showQuoteString (lookup1 a_default al)
                                      | k == ""
                                          = id
                                      | otherwise
                                          = showBlank
                                            . showString k

      showXmlDTD NOTATION al _cs
                                = showString "<!NOTATION "
                                  . showAttr a_name al
                                  . showExternalId al
                                  . showString " >"

      showXmlDTD PENTITY al cs  = showEntity "% " al cs

      showXmlDTD ENTITY al cs   = showEntity "" al cs

      showXmlDTD PEREF al _cs   = showPEAttr al

      showXmlDTD CONDSECT _ (c1 : cs)
                                = showString "<![ "
                                  . showXmlTree c1
                                  . showString " [\n"
                                  . showTrees cs
                                  . showString "]]>"

      showXmlDTD CONTENT al cs  = showContent (mkDTDElem CONTENT al cs)

      showXmlDTD NAME al _cs    = showAttr a_name al

      showXmlDTD de al _cs      = showString "NOT YET IMPLEMETED: "
                                  . showString (show de)
                                  . showBlank
                                  . showString (show al)
                                  . showString " [...]\n"

      -- ------------------------------------------------------------

      showEntity                :: String -> Attributes -> XmlTrees -> StringFct
      showEntity kind al cs     = showString "<!ENTITY "
                                  . showString kind
                                  . showAttr a_name al
                                  . showExternalId al
                                  . showNData al
                                  . showEntityValue cs
                                  . showString " >"


      showEntityValue           :: XmlTrees -> StringFct
      showEntityValue []        = id
      showEntityValue cs        = showBlank
                                  . showQuot
                                  . af (xshow cs)
                                  . showQuot

      -- ------------------------------------------------------------

      showContent               :: XmlTree -> StringFct
      showContent (NTree (XDTD de al) cs)
                                = cont2String de
                                  where
                                  cont2String           :: DTDElem -> StringFct
                                  cont2String NAME      = showAttr a_name al
                                  cont2String PEREF     = showPEAttr al
                                  cont2String CONTENT   = showLpar
                                                          . foldr1
                                                                (combine (lookup1 a_kind al))
                                                                (map showContent cs)
                                                          . showRpar
                                                          . showAttr a_modifier al
                                  cont2String n         = error ("cont2string " ++ show n ++ " is undefined")
                                  combine k s1 s2       = s1
                                                          . showString ( if k == v_seq
                                                                         then ", "
                                                                         else " | "
                                                                       )
                                                          . s2

      showContent n             = showXmlTree n

      -- ------------------------------------------------------------

      showElemType              :: String -> XmlTrees -> StringFct
      showElemType t cs
          | t == v_pcdata       = showLpar . showString v_pcdata . showRpar
          | t == v_mixed
            &&
            (not . null) cs     = showLpar
                                  . showString v_pcdata
                                  . ( foldr (.) id
                                      . map (mixedContent . selAttrl . getNode)
                                    ) cs1
                                  . showRpar
                                  . showAttr a_modifier al1
          | t == v_mixed                              -- incorrect tree, e.g. after erronius pe substitution
                                = showLpar
                                  . showRpar
          | t == v_children
            &&
            (not . null) cs     = showContent (head cs)
          | t == v_children     = showLpar
                                  . showRpar
          | t == k_peref        = foldr (.) id
                                  . map showContent $ cs
          | otherwise           = showString t
          where
          [(NTree (XDTD CONTENT al1) cs1)] = cs

          mixedContent          :: Attributes -> StringFct
          mixedContent l        = showString " | " . showAttr a_name l . showPEAttr l

          selAttrl (XDTD _ as)  = as
          selAttrl (XText tex)  = [(a_name, tex)]
          selAttrl _            = []

-- ------------------------------------------------------------

showQName                       :: QName -> StringFct
showQName                       = qualifiedName'
{-# INLINE showQName #-}

-- ------------------------------------------------------------

showQuoteString                 :: String -> StringFct
showQuoteString s               = showQuot . showString s . showQuot

-- ------------------------------------------------------------

showAttr                        :: String -> Attributes -> StringFct
showAttr k al                   = showString (fromMaybe "" . lookup k $ al)

-- ------------------------------------------------------------

showPEAttr                      :: Attributes -> StringFct
showPEAttr al                   = showPE (lookup a_peref al)
    where
    showPE (Just pe)            = showChar '%'
                                  . showString pe
                                  . showChar ';'
    showPE Nothing              = id

-- ------------------------------------------------------------

showExternalId                  :: Attributes -> StringFct
showExternalId al               = id2Str (lookup k_system al) (lookup k_public al)
    where
    id2Str Nothing  Nothing     = id
    id2Str (Just s) Nothing     = showBlank
                                  . showString k_system
                                  . showBlank
                                  . showQuoteString s
    id2Str Nothing  (Just p)    = showBlank
                                  . showString k_public
                                  . showBlank
                                  . showQuoteString p
    id2Str (Just s) (Just p)    = showBlank
                                  . showString k_public
                                  . showBlank
                                  . showQuoteString p
                                  . showBlank
                                  . showQuoteString s

-- ------------------------------------------------------------

showNData                       :: Attributes -> StringFct
showNData al                    = nd2Str (lookup k_ndata al)
    where
    nd2Str Nothing              = id
    nd2Str (Just v)             = showBlank
                                  . showString k_ndata
                                  . showBlank
                                  . showString v

-- ------------------------------------------------------------

showBlank,
  showEq, showLt, showGt, showSlash, showQuot, showLpar, showRpar, showNL :: StringFct

showBlank       = showChar ' '
{-# INLINE showBlank #-}

showEq          = showChar '='
{-# INLINE showEq #-}

showLt          = showChar '<'
{-# INLINE showLt #-}

showGt          = showChar '>'
{-# INLINE showGt #-}

showSlash       = showChar '/'
{-# INLINE showSlash #-}

showQuot        = showChar '\"'
{-# INLINE showQuot #-}

showLpar        = showChar '('
{-# INLINE showLpar #-}

showRpar        = showChar ')'
{-# INLINE showRpar #-}

showNL          = showChar '\n'
{-# INLINE showNL #-}

showChar        :: Char -> StringFct
showChar        = (:)
{-# INLINE showChar #-}

showString      :: String -> StringFct
showString      = (++)
{-# INLINE showString #-}

concatMap'      :: (Char -> StringFct) -> String -> StringFct
concatMap' f    = foldr (\ x r -> f x . r) id
{-# INLINE concatMap' #-}

textEscapeXml' = concatMap textEscapeChar

textEscapeChar '<' = "&lt;"
textEscapeChar '>' = "&gt;"
textEscapeChar '&' = "&amp;"
textEscapeChar x   = [x]
