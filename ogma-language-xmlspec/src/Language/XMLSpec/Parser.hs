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
import Control.Monad.Except     (ExceptT (..), liftEither)
import Control.Monad.IO.Class   (liftIO)
import Text.XML.HXT.Core        (configSysVars, no, readString, runX,
                                 withValidate, (>>>))
import Text.XML.HXT.DOM.ShowXml (xshow)
import Text.XML.HXT.XPath       (getXPathTrees, parseXPathExpr)

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
data XMLFormat = XMLFormat
  { specInternalVars    :: Maybe String
  , specInternalVarId   :: String
  , specInternalVarExpr :: String
  , specInternalVarType :: Maybe String
  , specExternalVars    :: Maybe String
  , specExternalVarId   :: String
  , specExternalVarType :: Maybe String
  , specRequirements    :: String
  , specRequirementId   :: String
  , specRequirementDesc :: Maybe String
  , specRequirementExpr :: String
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
             -> XMLFormat                       -- ^ XPaths for spec locations.
             -> String                          -- ^ String containing XML
             -> ExceptT String IO (Spec a)
parseXMLSpec parseExpr xmlFormat value = do
  xmlFormatInternal <- liftEither $ parseXMLFormat xmlFormat

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
        let msgI = "Requirement name"
        reqId <- ExceptT $
                   listToEither msgI <$>
                   executeXPath (xfiRequirementId xmlFormatInternal) def

        let msgE = "Requirement expression"
        reqExpr <- ExceptT $
                     listToEither msgE <$>
                     executeXPath (xfiRequirementExpr xmlFormatInternal) def
        reqExpr' <- parseExpr reqExpr

        let msgD = "Requirement description"
        reqDesc <- maybe
                     (liftEither $ Right "")
                     (\e -> ExceptT $ listToEither msgD <$> executeXPath e def)
                     (xfiRequirementDesc xmlFormatInternal)

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
  , xfiRequirementExpr :: XPathExpr
  }

-- | Internal representation of an XPath expression.
type XPathExpr = String

-- | Check an XMLFormat and return an internal representation.
--
-- Fails with an error message if any of the given expressions are not a valid
-- XPath expression.
parseXMLFormat :: XMLFormat -> Either String XMLFormatInternal
parseXMLFormat xmlFormat = do
    xfi2  <- swapMaybeEither $ checkXPathExpr <$> specInternalVars xmlFormat
    xfi3  <- checkXPathExpr $ specInternalVarId xmlFormat
    xfi4  <- checkXPathExpr $ specInternalVarExpr xmlFormat
    xfi5  <- swapMaybeEither $ checkXPathExpr <$> specInternalVarType xmlFormat
    xfi6  <- swapMaybeEither $ checkXPathExpr <$> specExternalVars xmlFormat
    xfi7  <- checkXPathExpr $ specExternalVarId xmlFormat
    xfi8  <- swapMaybeEither $ checkXPathExpr <$> specExternalVarType xmlFormat
    xfi9  <- checkXPathExpr $ specRequirements xmlFormat
    xfi10 <- checkXPathExpr $ specRequirementId xmlFormat
    xfi11 <- swapMaybeEither $ checkXPathExpr <$> specRequirementDesc xmlFormat
    xfi12 <- checkXPathExpr $ specRequirementExpr xmlFormat
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
  where
    checkXPathExpr :: String -> Either String XPathExpr
    checkXPathExpr s = s <$ parseXPathExpr s

-- | Execute an XPath query in an XML string, returning the list of strings
-- that match the path.
executeXPath :: String -> String -> IO [String]
executeXPath query string = do
  let config = [withValidate no]
  v <- runX $ configSysVars config >>> (readString [] string >>> getXPathTrees query)

  -- We apply xshow to every tree to turn it back into a string. That will
  -- allow us to use the same function to select subparts of the matched
  -- expression.
  return $ map (xshow . (:[])) v

-- * Auxiliary

-- | Swap the Maybe and Either layers of a value.
swapMaybeEither :: Maybe (Either a b) -> Either a (Maybe b)
swapMaybeEither Nothing          = Right Nothing
swapMaybeEither (Just (Left s))  = Left s
swapMaybeEither (Just (Right x)) = Right $ Just x

-- | Convert a list to an Either, failing if the list provided does not have
-- exactly one value.
listToEither :: String -> [String] -> Either String String
listToEither _   [x] = Right x
listToEither msg []  = Left $ "Failed to find a value for " ++ msg
listToEither msg _   = Left $ "Unexpectedly found multiple values for " ++ msg
