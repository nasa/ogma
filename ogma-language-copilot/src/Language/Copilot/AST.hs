module Language.Copilot.AST where

data Module f = Module (f (Ident f)) (f [f (Import f)]) (f [f (Def f)])

data Def f = Def (f (Maybe (f (DefSignature f)))) (f (DefBody f))

data DefSignature f =
  DefSignature (f (Ident f)) (f (Type f))

data Type f = PlainType (f (Ident f))
            | ArrayType (f Integer) (f (Type f))
            | FunType (f (Type f)) (f (Type f))

data DefBody f = DefBody (f (Ident f)) (f [f (Ident f)]) (f (Stream f)) (f [f (LocalDef f)])

data LocalDef f = LocalDef (f ([f (Def f)]))

data Stream f
  = StreamPar    (f (Stream f))
  | StreamIdent  (f (Ident f))     (f (ArgList f))
  | ConstStream  (f (Value f))
  | ExternStream (f String)        (f (Maybe (f (SampleV f))))
  | StreamAppend (f (ValueList f)) (f (Stream f))
  | StreamDrop   (f Integer)       (f (Stream f))
  | StreamOP1    (f OPOne)         (f (Stream f))
  | StreamOP2    (f OPTwo)         (f (Stream f)) (f (Stream f))
  | StreamOP3    (f OPThree)       (f (Stream f)) (f (Stream f)) (f (Stream f))
  | StreamStruct (f (Stream f))    (f (Ident f))

data SampleV f = SampleV (f (ValueList f))

type ValueList f = [f (Value f)]
type ArgList f   = [f (Argument f)]

data Value f
  = ValueBool  (f Bool)
  | ValueFloat (f Float)
  | ValueInt   (f Integer)
  | ValueArray (f (ValueList f))
  | ValueUID   (f (Ident f)) (f [Field f])

data Argument f
  = ArgValue (f (Value f))
  | ArgStream (f (Stream f))

data Field f = MkField (Value f)

data Import f =
  Import (f Bool) (f (Ident f)) (f (Maybe (f (Ident f)))) (f (Maybe (f (ImpsSpec f))))

data ImpsSpec f
  = HidingImp (f [f (ImportElem f)])
  | ExplImp (f [f (ImportElem f)])

data ImportElem f = ImportElem (f (Ident f)) (f (Maybe (f (ConElems f))))

data ConElems f
  = ConElemAll | ConElemSome [Var f]

data Var f = VarId (f (Ident f)) | VarSym (f SYMBOL)

newtype Ident f = Ident (f String)

newtype SYMBOL = SYMBOL String

newtype OPOne = OPOne String

newtype OPTwo = OPTwo String

newtype OPThree = OPThree String
