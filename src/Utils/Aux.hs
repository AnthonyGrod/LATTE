{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Utils.Aux where

import Parser.Abs


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
