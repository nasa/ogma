{-# LANGUAGE FlexibleInstances #-}
module Language.Copilot.Pretty where

import Data.List (intersperse)
import Data.Functor.Identity
import Language.Copilot.AST as AST

class Pretty f where
  pretty :: f -> String

instance Pretty (Module Identity) where
  pretty (Module (Identity i) (Identity is) (Identity ds)) =
    unlines
      $ [ "module " ++ pretty i ++ "where"]
      ++ [""]
      ++ map (pretty . runIdentity) is
      ++ [""]
      ++ map (pretty . runIdentity) ds

instance Pretty (Ident Identity) where
  pretty (Ident (Identity s)) = s

instance Pretty (Import Identity) where
  pretty (Import (Identity qual) (Identity mod) (Identity asQ) (Identity impsSecOpt)) =
      "import " ++ qualS ++ pretty mod ++ asQS ++ " " ++ impsSecOptS
    where
      qualS | qual      = "qualified "
            | otherwise = ""

      asQS  = maybe "" (\(Identity i) -> " as " ++ pretty i) asQ

      impsSecOptS = maybe "" ((" " ++) . pretty . runIdentity) impsSecOpt

instance Pretty (ImpsSpec Identity) where
  pretty (HidingImp (Identity es)) = " hiding (" ++ concat (intersperse ", " (map (pretty . runIdentity) es)) ++ ")"
  pretty (ExplImp (Identity es))   = " (" ++ concat (intersperse ", " (map (pretty . runIdentity) es)) ++ ")"

instance Pretty (ImportElem Identity) where
  pretty (ImportElem (Identity i) (Identity elems)) =
      pretty i ++ elemsS
    where
      elemsS = case elems of
                 Nothing -> ""
                 Just (Identity (ConElemAll)) -> "(..)"
                 Just (Identity (ConElemSome vs)) ->
                     let vsS = concat $ intersperse ", " (map pretty vs)
                     in " (" ++ vsS ++ ")"

instance Pretty (Var Identity) where
  pretty (VarId (Identity i))  = pretty i
  pretty (VarSym (Identity s)) = pretty s

instance Pretty SYMBOL where
  pretty (SYMBOL s) = s

instance Pretty (Def Identity) where
  pretty (StreamDef (Identity signatureOpt) (Identity body)) =
      signatureOptS ++ pretty body
    where
      signatureOptS = case signatureOpt of
                        Nothing           -> ""
                        Just (Identity s) -> pretty s ++ "\n"

  pretty (SpecDef (Identity ts)) =
    unlines $
      [ "spec :: Spec"
      , "spec = do"
      ]
      ++
      map ("  " ++) (map (pretty . runIdentity) ts)

  pretty (MainDef (Identity s)) =
    unlines $
      [ "main :: IO ()"
      , "main = do"
      , "  reify spec >>= compile " ++ show s
      ]

instance Pretty (Trigger Identity) where
  pretty (Trigger (Identity n) (Identity s) (Identity as)) =
      "trigger " ++ show n ++ " " ++ pretty s ++ " " ++ asS
    where
      asS = "[" ++ concat (intersperse "," (map ((\x -> "arg (" ++ x ++ ")") . pretty . runIdentity) as)) ++ "]"

instance Pretty (DefSignature Identity) where
  pretty (DefSignature (Identity i) (Identity t)) =
    pretty i ++ " :: " ++ pretty t

instance Pretty (Type Identity) where
  pretty (PlainType (Identity i)) =
    pretty i
  pretty (ArrayType (Identity len) (Identity ty)) =
    "Array " ++ show len ++ " " ++ pretty ty
  pretty (FunType (Identity ty1) (Identity ty2)) =
    pretty ty1 ++ " -> " ++ pretty ty2

instance Pretty (DefBody Identity) where
  pretty (DefBody (Identity i) (Identity as) (Identity s) (Identity wheres)) =
      unlines $
        [ pretty i ++ " " ++ asS ++ "= " ++ pretty s
        ]
        ++ wheresS
    where
      asS      = concat $ intersperse " " $ map (pretty . runIdentity) as
      wheresS  = if null wheresS' then [] else [ "  where"] ++ map ("    " ++) wheresS'
      wheresS' = map (pretty . runIdentity) wheres

instance Pretty (LocalDef Identity) where
  pretty (LocalDef (Identity ds)) = unlines $ map (pretty . runIdentity) ds

instance Pretty (Stream Identity) where
  pretty (StreamPar (Identity s)) = "(" ++ pretty s ++ ")"
  pretty (StreamIdent (Identity i) (Identity as)) =
      pretty i ++ asS
    where
      asS = concat $ map ((" " ++) . pretty . runIdentity) as
  pretty (ConstStream (Identity v)) =
    "const " ++ pretty v

  pretty (ExternStream (Identity s) (Identity samplesOpt)) =
    "extern " ++ show s ++ " " ++ samplesOptS
    where
      samplesOptS = case samplesOpt of
                      Nothing           -> "Nothing"
                      Just (Identity x) -> "(Just " ++ pretty x ++ ")"

  pretty (StreamAppend (Identity vs) (Identity s)) =
      vsS ++ " ++ " ++ pretty s
    where
      vsS = "[" ++ concat (intersperse ", " (map (pretty . runIdentity) vs)) ++ "]"

  pretty (StreamDrop (Identity i) (Identity s)) =
      "drop " ++ show i ++ " " ++ pretty s

  pretty (StreamOP1 (Identity op) (Identity s)) =
    pretty op ++ " " ++ pretty s

  pretty (StreamOP2 (Identity op) (Identity s1) (Identity s2)) =
    pretty op ++ " " ++ pretty s1 ++ " " ++ pretty s2

  pretty (StreamOP3 (Identity op) (Identity s1) (Identity s2) (Identity s3)) =
    pretty op ++ " " ++ pretty s1 ++ " " ++ pretty s2 ++ " " ++ pretty s3

  pretty _ = error "Not implemented"
  -- | StreamStruct (f (Stream f))    (f (Ident f))

instance Pretty OPOne where
  pretty (OPOne s) = s

instance Pretty OPTwo where
  pretty (OPTwo s) = s

instance Pretty OPThree where
  pretty (OPThree s) = s

instance Pretty (Argument Identity) where
  pretty (ArgValue (Identity v)) = pretty v
  pretty (ArgStream (Identity s)) = pretty s

instance Pretty (SampleV Identity) where
  pretty (SampleV (Identity vs)) = concat $ intersperse ", " (map (pretty . runIdentity) vs)

instance Pretty (Value Identity) where
  pretty (ValueBool (Identity v)) = show v
  pretty (ValueFloat (Identity v)) = show v
  pretty (ValueInt (Identity v)) = show v
  pretty _ = error "Not implemented"

-- data Value f
--   = ValueBool  (f Bool)
--   | ValueFloat (f Float)
--   | ValueInt   (f Integer)
--   | ValueArray (f (ValueList f))
--   | ValueUID   (f (Ident f)) (f [Field f])
--
-- type ValueList f = [f (Value f)]
--
-- data Field f = MkField (Value f)
