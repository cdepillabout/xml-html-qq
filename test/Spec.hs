{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (SomeException)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.XML (Document(..), Element(..), Prologue(..))

import Text.XML.QQ (xmlRaw, xmlUnsafe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [formTests]

formTests :: TestTree
formTests = testGroup "form" [formWorks]

formWorks :: TestTree
formWorks =
  testCase "works correctly" $
  test @?=
  Document
  { documentPrologue =
      Prologue
      {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}
  , documentRoot =
      Element
      {elementName = "html", elementAttributes = mempty, elementNodes = []}
  , documentEpilogue = []
  }

-- test :: Either SomeException Document
test :: Document
test = [xmlUnsafe|<html></html>|]

test2 :: Document
test2 = [xmlRaw|<html></html>|]
