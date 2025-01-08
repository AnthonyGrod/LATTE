{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Utils.Types where

import Parser.Abs
import Utils.Aux

type Label = Int
type Register = Int
type RegisterAndType = (Register, LLVMType)

showRegister :: Register -> String
showRegister = ("%" ++) . show

showLabel :: Label -> String
showLabel = ("Label" ++) . show

dummyReturnRegisterAndType :: (Register, LLVMType)
dummyReturnRegisterAndType = (0, TVVoid)

data LLVMValue =
  EVInt Int |
  EVBool Bool |
  EVString String |
  EVVoid |
  EVFun LLVMType Ident [(LLVMType, LLVMValue)] Block | -- return type + name + (arg type, arg register) + block
  EVLabel Int |
  EVReg Int
  deriving (Eq)

data LLVMType =
  TVInt |
  TVBool |
  TVString |
  TVVoid |
  TVLabel
  deriving (Eq)

bnfcTypeToLLVMType :: Type -> LLVMType
bnfcTypeToLLVMType (Int _) = TVInt
bnfcTypeToLLVMType (Str _) = TVString
bnfcTypeToLLVMType (Bool _) = TVBool
bnfcTypeToLLVMType (Void _) = TVVoid
bnfcTypeToLLVMType (Fun _ retType _) = bnfcTypeToLLVMType retType

instance Show LLVMType where
  show TVInt = "i32"
  show TVBool = "i1"
  show TVString = "i8*"
  show TVVoid = "void"
  show TVLabel = "label"

instance Show LLVMValue where
  show (EVInt i) = "add i32 0, " ++ show i
  show (EVBool b) = if b then "or i1 true, true" else "or i1 false, false"
  show (EVString s) = s
  show EVVoid = "void"
  show (EVFun retType ident args block) = show retType ++ " @" ++ show ident ++ "(" ++ show args ++ ") {\n" ++ show block ++ "\n}"
  show (EVLabel i) = "label %" ++ show i
  show (EVReg i) = "%R" ++ show i

getValueDefaultInit :: LLVMType -> LLVMValue
getValueDefaultInit TVInt = EVInt 0
getValueDefaultInit TVBool = EVBool False
getValueDefaultInit TVString = EVString ""
getValueDefaultInit TVVoid = EVVoid
getValueDefaultInit TVLabel = EVLabel 0

data DBinOp = BAdd | BSub | BMul | BDiv | BMod | BAnd | BOr deriving (Eq)
data DRelOp = RLTH | RLE | RGTH | RGE | RQU | RE deriving (Eq)

data Instr =
  IFunPr LLVMType Ident [(LLVMType, LLVMValue)] |           -- function definition (ret type + name + [(arg type, arg register)])
  IAss   LLVMValue LLVMValue | -- assign Reg = Value
  IBinOp LLVMValue LLVMValue LLVMValue DBinOp | -- binary operation
  IRelOp LLVMValue LLVMValue LLVMValue DRelOp | -- relational operation
  IFunEp  |
  IFunRet LLVMValue LLVMType |
  IBr LLVMValue Label Label |
  IBrJump Label |
  ILabel Label |
  IPhi LLVMValue LLVMType (LLVMValue, Label) (LLVMValue, Label) |
  IFunCall LLVMValue LLVMType Ident [(LLVMType, LLVMValue)] |
  IFunCallVoid Ident [(LLVMType, LLVMValue)] |
  IFunDecl LLVMType Ident [LLVMType]
  deriving (Eq)

instance Show DBinOp where
  show BAdd = "add"
  show BSub = "sub"
  show BMul = "mul"
  show BDiv = "sdiv"
  show BMod = "srem"
  show BAnd = "and"
  show BOr = "or"

instance Show DRelOp where
  show RLTH = "icmp slt"
  show RLE = "icmp sle"
  show RGTH = "icmp sgt"
  show RGE = "icmp sge"
  show RQU = "icmp eq"
  show RE = "icmp ne"

fromFunDeclToFunValue :: Instr -> LLVMValue
fromFunDeclToFunValue (IFunDecl retType ident args) = EVFun retType ident (map (\arg -> (arg, getValueDefaultInit arg)) args) (Block BNFC'NoPosition [])

fromFnDefToFunValue :: TopDef -> LLVMValue
fromFnDefToFunValue (FnDef _ retType ident args block) = EVFun (bnfcTypeToLLVMType retType) ident (map (\(Arg _ argType argIdent) -> (bnfcTypeToLLVMType argType, getValueDefaultInit (bnfcTypeToLLVMType argType))) args) block

fromLLVMValueToInstr :: LLVMValue -> Instr
fromLLVMValueToInstr (EVFun retType ident args block) = IFunDecl retType ident (map fst args)

getTypeFromTopDef :: TopDef -> Type
getTypeFromTopDef (FnDef _ retType _ _ _) = retType

builtInFunctions :: [Instr]
builtInFunctions =
  [ IFunDecl TVVoid (Ident "printString") [TVString]
  , IFunDecl TVVoid (Ident "printInt") [TVInt]
  , IFunDecl TVInt (Ident "readInt") []
  , IFunDecl TVString (Ident "readString") []
  , IFunDecl TVVoid (Ident "error") []
  -- , IFunDecl TVInt (Ident "_strlen") [TVString] -- TODO while implementing strings
  -- , IFunDecl TVString (Ident "_strcat") [TVString, TVString]
  -- , IFunDecl TVInt (Ident "_strcmp") [TVString, TVString]
  -- , IFunDecl TVString (Ident "_strcpy") [TVString, TVString]
  ]

instance Show Instr where
  show (IFunPr retType ident args) =
    "define " ++ show retType ++ " @" ++ extractIdent ident ++ "(" ++ case args of
      [] -> ") {"
      _  -> concatMap (\arg -> show (fst arg) ++ " " ++ show (snd arg) ++ ", ") (init args) ++ show (fst (last args)) ++ " " ++ show (snd (last args)) ++ ") {"
  show IFunEp = "}"
  show (IFunRet val retType) =
    "ret " ++ show retType ++ " " ++ show val
  show (IAss reg val) =
    show reg ++ " = " ++ show val
  show (IBinOp dest op1 op2 binOp) =
    case binOp of
      BAnd -> show dest ++ " = and i1 " ++ show op1 ++ ", " ++ show op2
      BOr -> show dest ++ " = or i1 " ++ show op1 ++ ", " ++ show op2
      _ -> show dest ++ " = " ++ show binOp ++ " i32 " ++ show op1 ++ ", " ++ show op2
  show (IRelOp dest op1 op2 relOp) =
    show dest ++ " = " ++ show relOp ++ " i32 " ++ show op1 ++ ", " ++ show op2
  show (ILabel label) = showLabel label ++ ":"
  show (IBr cond trueLabel falseLabel) =
    "br i1 " ++ show cond ++ ", label %" ++ showLabel trueLabel ++ ", label %" ++ showLabel falseLabel
  show (IBrJump label) = "br label %" ++ showLabel label
  show (IPhi lhsReg typ (val1, label1) (val2, label2)) =
    show lhsReg ++ " = phi " ++ show typ ++ " [" ++ show val1 ++ ", %"
    ++ showLabel label1 ++ "], [" ++ show val2 ++ ", %"
    ++ showLabel label2 ++ "]"
  show (IFunCall reg retType ident args) =
    show reg ++ " = call " ++ show retType ++ " @" ++ extractIdent ident ++ "(" ++ case args of
      [] -> ")"
      _  -> concatMap (\arg -> show (fst arg) ++ " " ++ show (snd arg) ++ ", ") (init args) ++ show (fst (last args)) ++ " " ++ show (snd (last args)) ++ ")"
  show (IFunCallVoid ident args) =
    "call void @" ++ extractIdent ident ++ "(" ++ case args of
      [] -> ")"
      _  -> concatMap (\arg -> show (fst arg) ++ " " ++ show (snd arg) ++ ", ") (init args) ++ show (fst (last args)) ++ " " ++ show (snd (last args)) ++ ")"
  show (IFunDecl retType ident args) = "declare " ++ show retType ++ " @" ++ extractIdent ident ++ "(" ++ case args of
    [] -> ")"
    _  -> concatMap (\arg -> show arg ++ ", ") (init args) ++ show (last args) ++ ")"
  show _ = "+++++++++"
