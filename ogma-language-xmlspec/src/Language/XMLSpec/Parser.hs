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
import Control.Monad.Except   (ExceptT (..), liftEither, throwError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List              (isPrefixOf)
import Data.Maybe             (fromMaybe, listToMaybe)
import Text.XML.HXT.Core      (configSysVars, no, readString, runX,
                               withCanonicalize, withOutputPLAIN, withRedirect,
                               withRemoveWS, withSubstDTDEntities,
                               withSubstHTMLEntities, withValidate, yes, (>>>))
import Text.XML.HXT.XPath     (getXPathTrees, parseXPathExpr)

-- External imports: ogma-spec
import Data.OgmaSpec (ExternalVariableDef (..), InternalVariableDef (..),
                      Requirement (..), Spec (Spec))

-- Internal imports
import Language.XMLSpec.PrintTrees (pretty, flattenDoc)

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
  deriving (Show, Read)

-- | Parse an XML file and extract a Spec from it.
--
-- An auxiliary function must be provided to parse the requirement expressions.
--
-- Fails if any of the XPaths in the argument XMLFormat are not valid
-- expressions, of the XML is malformed, or if the elements are not found with
-- the frequency expected (e.g., an external variable id is not found even
-- though external variables are found).
parseXMLSpec :: (String -> IO (Either String a)) -- ^ Parser for expressions.
             -> a
             -> XMLFormat                        -- ^ XPaths for spec locations.
             -> String                           -- ^ String containing XML
             -> IO (Either String (Spec a))
parseXMLSpec parseExpr defA xmlFormat value = runExceptT $ do
  xmlFormatInternal <- parseXMLFormat xmlFormat value

  -- Internal variables

  -- intVarStrings :: [String]
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

  -- extVarStrings :: [String]
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

  -- reqStrings :: [String]
  reqStrings <- liftIO $ executeXPath (xfiRequirements xmlFormatInternal) value

  let -- requirementDef :: String -> ExceptT String (Requirement a)
      requirementDef def = do
        -- let msgI = "Requirement name: " ++ take 160 def
        reqId <- liftIO $ fromMaybe "" . listToMaybe <$>
                   executeXPath (xfiRequirementId xmlFormatInternal) def

        -- let msgE = "Requirement expression: " ++ take 160 def
        reqExpr <- liftIO $
                     listToMaybe <$>
                       concatMapM (`executeXPath` def) (xfiRequirementExpr xmlFormatInternal)

        reqExpr' <- maybe (return defA)
                          (ExceptT . parseExpr . textUnescape)
                          reqExpr

        -- let msgD = "Requirement description"
        reqDesc <- maybe
                     (liftEither $ Right "")
                     (\e -> liftIO $ fromMaybe "" . listToMaybe <$> executeXPath e def)
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
  , xfiRequirementExpr :: [XPathExpr]
  }

-- | Internal representation of an XPath expression.
type XPathExpr = String

-- | Resolve an indirect XPath query, returning an XPath expression.
resolveIndirectly :: String
                  -> (String, Maybe (String, String))
                  -> ExceptT String IO XPathExpr
resolveIndirectly _ (query, Nothing) =
  liftEither $ checkXPathExpr query

resolveIndirectly xml (query, Just (key, val)) = do
  -- Check that the given query string parses correctly.
  _ <- liftEither $ checkXPathExpr val

  v  <- liftIO $ executeXPath val xml
  case v of
    (f:_) -> do let query' = replace query key f
                liftEither $ checkXPathExpr query'
    _     -> throwError $
               "Substitution path " ++ show val ++ " not found in file."

-- | Resolve an indirect XPath query, returning a list of XPath expressions.
resolveIndirectly' :: String
                   -> (String, Maybe (String, String))
                   -> ExceptT String IO [XPathExpr]
resolveIndirectly' _ (query, Nothing) =
  fmap (:[]) $ liftEither $ checkXPathExpr query

resolveIndirectly' xml (query, Just (key, val)) = do
  -- Check that the given query string parses correctly.
  _ <- liftEither $ checkXPathExpr val

  v  <- liftIO $ executeXPath val xml
  case v of
    [] -> throwError $ "Substitution path " ++ show val ++ " not found in file."
    fs -> do let queries = map (replace query key) fs
             liftEither $ mapM checkXPathExpr queries

-- | Check that an XPath expression is syntactically correct.
checkXPathExpr :: String -> Either String XPathExpr
checkXPathExpr s = s <$ parseXPathExpr s

-- | Check an XMLFormat and return an internal representation.
--
-- Fails with an error message if any of the given expressions are not a valid
-- XPath expression.
parseXMLFormat :: XMLFormat -> String -> ExceptT String IO XMLFormatInternal
parseXMLFormat xmlFormat file = do
  xfi2  <- liftEither $ swapMaybeEither
                      $ checkXPathExpr <$> specInternalVars xmlFormat

  xfi3  <- resolveIndirectly file $ specInternalVarId xmlFormat

  xfi4  <- resolveIndirectly file $ specInternalVarExpr xmlFormat

  xfi5  <- swapMaybeExceptT $
             resolveIndirectly file <$> specInternalVarType xmlFormat

  xfi6  <- liftEither $
             swapMaybeEither $ checkXPathExpr <$> specExternalVars xmlFormat

  xfi7  <- resolveIndirectly file $ specExternalVarId xmlFormat

  xfi8  <- swapMaybeExceptT $
             resolveIndirectly file <$> specExternalVarType xmlFormat

  xfi9  <- resolveIndirectly file $ specRequirements xmlFormat

  xfi10 <- resolveIndirectly file $ specRequirementId xmlFormat

  xfi11 <- swapMaybeExceptT $
             resolveIndirectly file <$> specRequirementDesc xmlFormat

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
  let config = [ withValidate no
               , withRedirect no
               , withCanonicalize no
               , withRemoveWS yes
               , withSubstDTDEntities no
               , withOutputPLAIN
               , withSubstHTMLEntities no
               ]
  v <- runX $ configSysVars config
              >>> (readString config string >>> getXPathTrees query)

  let u = map (flattenDoc . pretty . (:[])) v

  return u

-- * Auxiliary

-- | Unescape @'<'@, @'>'@ and @'&'@ in a string.
textUnescape :: String -> String
textUnescape ('&':'l':'t':';':xs)        = '<' : textUnescape xs
textUnescape ('&':'g':'t':';':xs)        = '>' : textUnescape xs
textUnescape ('&':'a':'m': 'p' : ';':xs) = '&' : textUnescape xs
textUnescape (x:xs)                      = x : textUnescape xs
textUnescape []                          = []

-- | Swap the Maybe and Either layers of a value.
swapMaybeEither :: Maybe (Either a b) -> Either a (Maybe b)
swapMaybeEither Nothing          = Right Nothing
swapMaybeEither (Just (Left s))  = Left s
swapMaybeEither (Just (Right x)) = Right $ Just x

-- | Swap the Maybe and Either layers of a value.
swapMaybeExceptT :: Monad m => Maybe (ExceptT a m b) -> ExceptT a m (Maybe b)
swapMaybeExceptT Nothing  = return Nothing
swapMaybeExceptT (Just e) = Just <$> e

-- | Convert a list to an Either, failing if the list provided does not have
-- exactly one value.
listToEither :: String -> [String] -> Either String String
listToEither _   [x] = Right x
listToEither msg []  = Left $ "Failed to find a value for " ++ msg
listToEither msg _   = Left $ "Unexpectedly found multiple values for " ++ msg

-- | Replace a string by another string
replace :: String -> String -> String -> String
replace []           _k  _v    = []
replace string@(h:t) key value
  | key `isPrefixOf` string
  = value ++ replace (drop (length key) string) key value
  | otherwise
  = h : replace t key value

-- | Map a monadic action over the elements of a container and concatenate the
-- resulting lists.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
