{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , xmlUnsafe
  , xmlRaw
  , Document
  ) where

import Control.FromSum (fromEither)
import Data.Default (def)
import Data.Text.Lazy (pack)
import Language.Haskell.TH (appE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Heterocephalus (compileFromString, textSetting)
import Text.XML (Document(..), parseText)

import Text.XMLHTML.Internal
       (createExpQuasiQuoter, handleParseDocErr)

-- | This 'QuasiQuoter' produces XML 'Document's.
--
-- The type of an expression that this 'QuasiQuoter' produces is @'Either' 'SomeException' 'Document'@.  It produces a @'Left' 'SomeException'@ when the document cannot be parsed.
xml :: QuasiQuoter
xml =
  createExpQuasiQuoter $ \string ->
    appE [|parseText def . renderMarkup|] $ compileFromString textSetting string

xmlUnsafe :: QuasiQuoter
xmlUnsafe =
  createExpQuasiQuoter $ \string ->
    appE
      [|fromEither (handleParseDocErr "XML" "Text.XML.parseText" string) .
        parseText def . renderMarkup|]
      (compileFromString textSetting string)

xmlRaw :: QuasiQuoter
xmlRaw =
  createExpQuasiQuoter $ \string ->
    let eitherDoc = parseText def $ pack string
    in either (handleParseDocErr "XML" "Text.XML.parseText" string) lift eitherDoc
