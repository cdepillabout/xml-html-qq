{- |
Module      :  Text.XML.QQ

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module provides a quasi-quoter for XML 'Document's.
-}

module Text.XML.QQ
  ( xml
  , Document
  ) where

import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Heterocephalus (compileFromString, textSetting)

xml :: QuasiQuoter
xml = QuasiQuoter
  { quoteExp = lala
  , quotePat = error "not used"
  , quoteType = error "not used"
  , quoteDec = error "not used"
  }

lala :: String -> Q Exp
lala string = do
  exp <- compileFromString textSetting string
  undefined
