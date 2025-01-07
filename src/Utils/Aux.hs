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