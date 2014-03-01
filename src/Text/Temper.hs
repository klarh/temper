{-# LANGUAGE OverloadedStrings #-}

module Text.Temper where

import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.Map as M
import Data.Maybe
import Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

data Element = Tag {tagName::Text,
                    ident::Maybe Text,
                    classes::[Text],
                    attributes::M.Map Text Text,
                    children::[Element]} |
               TextNode {nodeText::Text} |
               SpecialTag {specialTagContents::Text} deriving Show

emptyTag::Element
emptyTag = Tag "" Nothing [] M.empty []

tagModifier::Parser (Element->Element)
tagModifier = do
  c <- oneOf ['.', '#']
  n <- many1 . noneOf $ [' ', '.', '#', '\r', '\n']
  case c of
    '.' -> return (\elt -> elt {classes=(T.pack n:classes elt)})
    '#' -> return (\elt -> elt {ident=Just . pack $ n})
    _ -> unexpected "Unhandled tag modifier case"

tagStart::Parser Element
tagStart = do
  tname <- many1 . noneOf $ [' ', '.', '#', '\r', '\n']
  mods <- many tagModifier
  return (Prelude.foldr (.) id mods $ emptyTag {tagName=T.pack tname})

quote = oneOf ['"', '\'']

onlySpace = oneOf [' ', '\t']

tagAttribute::Parser (Text, Text)
tagAttribute = do
  name <- many1 . noneOf $ ['=', ' ', '$']
  _ <- char '='
  value <- between quote quote (many . noneOf $ ['"', '\''])
  return (T.pack name, T.pack value)

tagAttributes::Parser (M.Map Text Text)
tagAttributes = do
  pairs <- try tagAttribute `sepEndBy` many1 onlySpace
  return (M.fromList pairs)

tag::Int->Parser Element
tag minimumIndent = do
  _ <- Text.Parsec.count minimumIndent onlySpace
  addIndent <- Prelude.length <$> many onlySpace
  initialTag <- tagStart
  _ <- many onlySpace
  attributes <- tagAttributes
  textChild <- optionMaybe (many onlySpace >> textNode 0)
  children'' <- elements' (minimumIndent + addIndent + 1)
  let children' = case textChild of
        Just c -> c:children''
        _ -> children''
  return (initialTag {classes=Prelude.reverse . classes $ initialTag, attributes=attributes, children=children'})

closedTag::Int->Parser Element
closedTag minimumIndent = do
  _ <- Text.Parsec.count minimumIndent onlySpace
  initialTag <- skipMany onlySpace >> tagStart
  attributes <- many onlySpace >> tagAttributes
  _ <- many onlySpace >> char '/' >> many onlySpace
  return (initialTag {classes=Prelude.reverse . classes $ initialTag, attributes=attributes, children=[]})

textNode::Int->Parser Element
textNode minimumIndent = do
  _ <- Text.Parsec.count minimumIndent onlySpace >> skipMany onlySpace >> char '$'
  rest <- many . noneOf $ ['\r', '\n']
  return (TextNode . T.pack $ rest)

specialTag::Parser Element
specialTag = do
  _ <- char '!'
  rest <- many . noneOf $ ['\r', '\n']
  return (SpecialTag . T.pack $ rest)

comment::Parser String
comment = do
  _ <- many onlySpace >> char '#'
  rest <- many . noneOf $ ['\r', '\n']
  return rest

emptyLine::Parser Char
emptyLine = many onlySpace >> newline

element::Int->Parser Element
element minimumIndent = do
  -- _ <- (many onlySpace <|> comment) `sepBy` newline
  skipMany . try $ (comment >> newline) <|> emptyLine
  result <- try specialTag <|> try (textNode minimumIndent) <|> try (closedTag minimumIndent) <|> try (tag minimumIndent)
  return result

elements'::Int->Parser [Element]
elements' minimumIndent = many . try $ element minimumIndent

elements::Parser [Element]
elements = elements' 0

encodeElement::Element->Text
encodeElement (Tag name ident classes attributes children) =
  T.concat ['<' `T.cons` name, ident', classes', attributes', rest]
  where
    rest
      | Prelude.null children = " />"
      | otherwise =  '>' `T.cons` children' `T.append` closure
    ident' = maybe T.empty (\x -> " id=\"" `T.append` x `T.snoc` '"') ident
    classes'
      | Prelude.null classes = T.empty
      | otherwise = " class=\"" `T.append` T.unwords classes `T.snoc` '"'
    attributes' = T.concat $ T.cons ' ' . translateAttr <$> M.toList attributes
    translateAttr (name, val) = name `T.append` "=\"" `T.append` val `T.snoc` '"'
    children' = T.unlines $ encodeElement <$> children
    closure = "</" `T.append` name `T.snoc` '>'
encodeElement (TextNode conts) = conts
encodeElement (SpecialTag conts) = "<!" `T.append` conts `T.snoc` '>'
