{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

-- | Parsing and printing positional tags and tagsets.

module Text.Tagset.Positional
( parseTag
, showTag
, parseTagset
) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Text.Parsec
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import Data.Tagset.Positional

-- | Parse the tag given the corresponding tagset.
parseTag :: Tagset -> T.Text -> Tag
parseTag tagset inp =
    Tag _pos . M.fromList $ parseRule (rule tagset _pos) attrVals
  where
    (_pos : attrVals) = T.split (==':') inp
    parseRule ((attr, opt):restAtts) (x:xs)
        | x `S.member` domain tagset attr
            = (attr, x) : parseRule restAtts xs
        | opt == True   = parseRule restAtts (x:xs)
        | otherwise     = error $ "parseRule:"
            ++ " no value for " ++ T.unpack attr
            ++ " attribute in tag " ++ T.unpack inp
    parseRule [] [] = []
    parseRule ((_, True):restAtts) [] = parseRule restAtts []
    parseRule _ [] = error $ "parseRule: unexpected end of input in tag "
        ++ T.unpack inp
    parseRule [] _ = error $ "parseRule: input too long in tag "
        ++ T.unpack inp

-- | Print the tag given the corresponding tagset.
showTag :: Tagset -> Tag -> T.Text
showTag tagset tag =
    T.intercalate ":" (pos tag : catMaybes attrVals)
  where
    attrVals = map showAttr $ rule tagset (pos tag)
    showAttr (attr, opt)
        | Just x <- M.lookup attr (atts tag) = Just x
        | opt == True = Nothing
        | otherwise = error $
            "showTag: no value for mandatory attribute " ++ T.unpack attr

-- | Below we defined the parser for the positional tagset.

type Parser = Parsec String ()

tagsetFile :: Parser Tagset
tagsetFile = spaces *> (Tagset <$> attrSec <*> ruleSec)

attrSec :: Parser (M.Map Attr (S.Set T.Text))
attrSec = do
    secName "ATTR" *> spaces
    defs <- attrLine `endBy` spaces
    return $ M.fromList defs

attrLine :: Parser (Attr, S.Set T.Text)
attrLine = do
    attr <- ident
    _ <- spaces *> char '=' *> lineSpaces
    values <- map T.pack <$> ident `endBy` lineSpaces
    return (T.pack attr, S.fromList values)

ruleSec :: Parser (M.Map POS [(Attr, Optional)])
ruleSec = do
    secName "RULE" *> spaces
    M.fromList <$> ruleLine `endBy` spaces

ruleLine :: Parser (POS, [(Attr, Optional)])
ruleLine = do
    _pos <- ident
    _ <- lineSpaces *> char '=' *> lineSpaces
    actionAtts <- attrName `endBy` lineSpaces
    return $ (T.pack _pos, actionAtts)

attrName :: Parser (Attr, Optional)
attrName = optionalAttrName <|> plainAttrName <?> "attribute name"

optionalAttrName :: Parser (Attr, Optional)
optionalAttrName = do
    name <- char '[' *> ident <*  char ']'
    return (T.pack name, True)

plainAttrName :: Parser (Attr, Optional)
plainAttrName = do
    name <- ident
    return $ (T.pack name, False)

lineSpace :: Parser Char
lineSpace = satisfy $ \c -> (isSpace c) && (not $ c == '\n')

lineSpaces :: Parser String
lineSpaces = many lineSpace

ident :: Parser String
ident = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "."

secName :: String -> Parser Char
secName name = char '[' *> string name *> char ']'

-- | Parse the textual representation of the tagset.  The first argument
-- should be the name of the source.
parseTagset :: String -> String -> Tagset
parseTagset src contents = do
    case parse tagsetFile src filtered of
        Left e  -> error $ "parseTagset: Error parsing input:\n" ++ show e
        Right r -> r
  where
     filtered = unlines $ map (removeComment '#') $ lines contents

removeComment :: Char -> String -> String
removeComment commChar s = case findComment s of
    Just i -> fst $ splitAt i s
    Nothing -> s
  where
    findComment xs = doFind xs 0 False
    doFind (x:xs) acc inQuot
        | x == commChar && not inQuot = Just acc
        | x == '"' = doFind xs (acc + 1) (not inQuot)
        | otherwise =  doFind xs (acc + 1) inQuot
    doFind [] _ _ = Nothing
