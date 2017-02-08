{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Text.XML.QQ

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module provides a quasi-quoter for XML 'Document's.  See the 'xml'
function for some examples.


-}

module Text.XML.QQ
  ( xml
  , xmlUnsafe
  , xmlRaw
    -- * Types
  , Document
  , SomeException
  ) where

import Control.Exception (SomeException)
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

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XTemplateHaskell


-- | This 'QuasiQuoter' produces XML 'Document's.
--
-- This 'QuasiQuoter' produces expressions of type
-- @'Either' 'SomeException' 'Document'@.  It produces a
-- @'Left' 'SomeException'@ when the input string cannot be parsed into an XML
-- 'Document'.
--
-- Here's a simple example of using it:
--
-- >>> [xml|<html></html>|]
-- Right (Document {documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}, documentRoot = Element {elementName = Name {nameLocalName = "html", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = fromList [], elementNodes = []}, documentEpilogue = []})
--
-- Internally, this function is using the
-- <https://hackage.haskell.org/package/heterocephalus heterocephalus> package.
-- This means you can use variable interpolation, as well as @forall@, @if@,
-- and @case@ control statements.  Checkout the
-- <https://github.com/arowM/heterocephalus#syntax heterocephalus README> for
-- more info.
--
-- >>> let a = "hello world"
-- >>> [xml|<html>#{a}</html>|]
-- Right ...
--
-- Here's an example of a template that will produce a 'Left' value:
--
-- >>> [xml|<html </html>|]
-- Left ...
--
-- Here's an example of a template that can be parsed as an HTML 'Document', but
-- not as an XML 'Document':
--
-- >>> [xml|<html><br></html>|]
-- Left ...
xml :: QuasiQuoter
xml =
  createExpQuasiQuoter $ \string ->
    appE [|parseText def . renderMarkup|] $ compileFromString textSetting string

-- | This function is just like 'xml', but produces expressions of type
-- 'Document'.
--
-- If your input string cannot be parsed into a valid 'Document', an error will
-- be thrown at runtime with 'error'.
--
-- This function is nice to use in GHCi or tests, but should not be used in
-- production code.
xmlUnsafe :: QuasiQuoter
xmlUnsafe =
  createExpQuasiQuoter $ \string ->
    appE
      [|fromEither (handleParseDocErr "XML" "Text.XML.parseText" string) .
        parseText def . renderMarkup|]
      (compileFromString textSetting string)

-- | This function is similar to 'xml', but doesn't allow variable interpolation
-- or control statements.  It produces expressions of type 'Document'.
--
-- An error will be thrown at compile-time if the input string cannot be parsed
-- into a 'Document'.
xmlRaw :: QuasiQuoter
xmlRaw =
  createExpQuasiQuoter $ \string ->
    let eitherDoc = parseText def $ pack string
    in either (handleParseDocErr "XML" "Text.XML.parseText" string) lift eitherDoc
