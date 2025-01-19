{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Aux where

import Parser.Abs

type Label = Int
type Register = Int
type StringNum = Int
type ValueAndType = (LLVMValue, LLVMType)

showRegister :: Register -> String
showRegister = ("%" ++) . show

showLabel :: Label -> String
showLabel = ("Label" ++) . show

dummyReturnValueAndType :: (LLVMValue, LLVMType)
dummyReturnValueAndType = (EVVoid, TVVoid)

data LLVMValue =
  EVInt Int |
  EVBool Bool |
  EVString String StringNum |
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
  TVLabel |
  TVReg
  deriving (Eq)

bnfcTypeToLLVMType :: Type -> LLVMType
bnfcTypeToLLVMType (Int _) = TVInt
bnfcTypeToLLVMType (Str _) = TVString
bnfcTypeToLLVMType (Bool _) = TVBool
bnfcTypeToLLVMType (Void _) = TVVoid
bnfcTypeToLLVMType (Fun _ retType _) = bnfcTypeToLLVMType retType

extractRegisterValue :: LLVMValue -> Int
extractRegisterValue (EVReg i) = i

instance Show LLVMType where
  show TVInt = "i32"
  show TVBool = "i1"
  show TVString = "i8*"
  show TVVoid = "void"
  show TVLabel = "label"
  show TVReg = "register"

instance Show LLVMValue where
  show (EVInt i) = show i
  show (EVBool b) = if b then "true" else "false"
  show (EVString s strNum) = "getelementptr " ++
                      "[" ++ show (length s + 1) ++ " x i8], "
                      ++ "[" ++ show (length s + 1) ++ " x i8]* @." ++ show strNum ++ ", i32 0, i32 0"
  show EVVoid = "void"
  show (EVFun retType ident args block) = show retType ++ " @" ++ show ident ++ "(" ++ show args ++ ") {\n" ++ show block ++ "\n}"
  show (EVLabel i) = "label %" ++ show i
  show (EVReg i) = "%R" ++ show i

getValueDefaultInit :: LLVMType -> LLVMValue
getValueDefaultInit TVInt = EVInt 0
getValueDefaultInit TVBool = EVBool False
getValueDefaultInit TVString = EVString "" 1
getValueDefaultInit TVVoid = EVVoid
getValueDefaultInit TVLabel = EVLabel 0

data DBinOp = BAdd | BSub | BMul | BDiv | BMod | BAnd | BOr deriving (Eq)
data DRelOp = RLTH | RLE | RGTH | RGE | RQU | RE deriving (Eq)

data Instr =
  IFunPr LLVMType Ident [(LLVMType, LLVMValue)] |
  IAss   LLVMValue LLVMValue |
  IBinOp LLVMValue LLVMValue LLVMValue DBinOp |
  IRelOp LLVMValue LLVMType LLVMValue LLVMValue DRelOp |
  IFunEp  |
  IFunRet LLVMValue LLVMType |
  IBr LLVMValue Label Label |
  IBrJump Label |
  ILabel Label |
  IPhi LLVMValue LLVMType (LLVMValue, Label) (LLVMValue, Label) |
  IFunCall LLVMValue LLVMType Ident [(LLVMType, LLVMValue)] |
  IFunCallVoid Ident [(LLVMType, LLVMValue)] |
  IFunDecl LLVMType Ident [LLVMType] |
  IStringGlobal Ident String
  deriving (Eq)

getBinOpRHS :: Instr -> [LLVMValue]
getBinOpRHS (IBinOp dest op1 op2 binOp) = [op1, op2]
getRelOpRHS :: Instr -> [LLVMValue]
getRelOpRHS (IRelOp dest destType op1 op2 relOp) = [op1, op2]
getAssRHS :: Instr -> LLVMValue
getAssRHS (IAss _ val) = val

getLHSReg :: Instr -> LLVMValue
getLHSReg (IBinOp dest _ _ _) = dest
getLHSReg (IRelOp dest _ _ _ _) = dest
getLHSReg (IAss dest _) = dest

areInstrsTheSameType :: Instr -> Instr -> Bool
areInstrsTheSameType (IBinOp _ _ _ op1) (IBinOp _ _ _ op2) = op1 == op2
areInstrsTheSameType (IRelOp _ _ _ _ op1) (IRelOp _ _ _ _ op2) = op1 == op2
areInstrsTheSameType (IAss _ _) (IAss _ _) = True
areInstrsTheSameType _ _ = False

replaceRegInInstrRHS :: LLVMValue -> LLVMValue -> Instr -> Instr
replaceRegInInstrRHS lhs lhs_x instr = case instr of
  IAss lhs' rhs -> IAss (if lhs' == lhs_x then lhs else lhs') rhs
  IBinOp dest op1 op2 binOp -> IBinOp dest (if op1 == lhs_x then lhs else op1) (if op2 == lhs_x then lhs else op2) binOp
  IRelOp dest destType op1 op2 relOp -> IRelOp dest destType (if op1 == lhs_x then lhs else op1) (if op2 == lhs_x then lhs else op2) relOp
  IFunRet val retType -> IFunRet (if val == lhs_x then lhs else val) retType
  IBr cond trueLabel falseLabel -> IBr (if cond == lhs_x then lhs else cond) trueLabel falseLabel
  IPhi lhsReg typ (val1, label1) (val2, label2) -> IPhi lhsReg typ (if val1 == lhs_x then lhs else val1, label1) (if val2 == lhs_x then lhs else val2, label2)
  IFunCall reg retType ident args -> IFunCall (if reg == lhs_x then lhs else reg) retType ident (map (\(argType, argVal) -> (argType, if argVal == lhs_x then lhs else argVal)) args)
  IFunCallVoid ident args -> IFunCallVoid ident (map (\(argType, argVal) -> (argType, if argVal == lhs_x then lhs else argVal)) args)
  instr -> instr

isAssignableOp :: Instr -> Bool
isAssignableOp (IAss _ _) = True
isAssignableOp (IBinOp dest _ _ _) = True
isAssignableOp (IRelOp dest _ _ _ _) = True
isAssignableOp _ = False

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
  , IFunDecl TVString (Ident "_strcat") [TVString, TVString]
  , IFunDecl TVBool (Ident "_strcmp") [TVString, TVString]
  , IFunDecl TVVoid (Ident "increaseRefCount") [TVString]
  , IFunDecl TVVoid (Ident "decreaseRefCount") [TVString]
  ]

instance Show Instr where
  show (IFunPr retType ident args) =
    "define " ++ show retType ++ " @" ++ extractIdent ident ++ "(" ++ case args of
      [] -> ") {"
      _  -> concatMap (\arg -> show (fst arg) ++ " " ++ show (snd arg) ++ ", ") (init args) ++ show (fst (last args)) ++ " " ++ show (snd (last args)) ++ ") {"
  show IFunEp = "}"
  show (IFunRet val retType) = case retType of
    TVVoid -> "ret void"
    _ -> "ret " ++ show retType ++ " " ++ show val
  show (IAss reg val) =
    case val of
      EVVoid -> ""
      EVInt i -> show reg ++ " = " ++ "add i32 0, " ++ show i
      EVBool b -> show reg ++ " = " ++ "add i1 0, " ++ (if b then "1" else "0")
      _ -> show reg ++ " = " ++ show val
  show (IBinOp dest op1 op2 binOp) =
    case binOp of
      BAnd -> show dest ++ " = and i1 " ++ show op1 ++ ", " ++ show op2
      BOr -> show dest ++ " = or i1 " ++ show op1 ++ ", " ++ show op2
      _ -> show dest ++ " = " ++ show binOp ++ " i32 " ++ show op1 ++ ", " ++ show op2
  show (IRelOp dest destType op1 op2 relOp) =
    case destType of
      TVInt -> show dest ++ " = " ++ show relOp ++ " i32 " ++ show op1 ++ ", " ++ show op2
      TVBool -> show dest ++ " = " ++ show relOp ++ " i1 " ++ show op1 ++ ", " ++ show op2
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
  show (IStringGlobal ident str) = "@." ++ extractIdent ident ++ " = private constant [" ++ show (length str + 1) ++ " x i8] c\"" ++ str ++ "\\00\""



extractIdent :: Ident -> String
extractIdent (Ident s) = s

extractIdentFromItem :: Item -> Ident
extractIdentFromItem (NoInit _ ident) = ident
extractIdentFromItem (Init _ ident _) = ident

isELitTrue :: Expr -> Bool
isELitTrue (ELitTrue _) = True
isELitTrue _            = False

isELitFalse :: Expr -> Bool
isELitFalse (ELitFalse _) = True
isELitFalse _             = False

doesBlockContainVRet :: Block -> Bool
doesBlockContainVRet (Block _ stmts) = any isVRet stmts
  where
    isVRet :: Stmt -> Bool
    isVRet (VRet _) = True
    isVRet _        = False
