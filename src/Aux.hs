{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Aux where

import Parser.Abs


extractIdent :: Ident -> String
extractIdent (Ident s) = s
