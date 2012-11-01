{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.Tagset.Positional
(
-- * Tagset
  Tagset (..)
, Attr
, AttrVal
, POS
, Optional
, domain
, rule
-- ** Parsing
, parseTagset

-- * Tag
, Tag (..)
, expand
, tagSim
-- ** Parsing and printing
, parseTag
, showTag
) where

import Control.Arrow (first)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Text.Parsec
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Binary (Binary, get, put)
import Data.Text.Binary ()
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

-- | Attribute name.
type Attr = T.Text

-- | Attribute name.
type AttrVal = T.Text

-- | Part of speech.
type POS = T.Text

-- | Is the attribute optional?
type Optional = Bool

-- | The tagset consists of a domain for each attribute name and of a
-- parsing rule for each part of speech.
data Tagset = Tagset
    { domains   :: M.Map Attr (S.Set AttrVal)
    , rules     :: M.Map POS  [(Attr, Optional)]
    } deriving (Show, Eq, Ord)

instance Binary Tagset where
    put Tagset{..} = put domains >> put rules
    get = Tagset <$> get <*> get

-- | Set of potential values for the given attribute.
domain :: Tagset -> Attr -> S.Set AttrVal
domain Tagset{..} x =
  case x `M.lookup` domains of
    Just y  -> y
    Nothing -> error $ "domain: unknown attribute " ++ T.unpack x

-- | Parsing rule for the given POS.
rule :: Tagset -> POS -> [(Attr, Optional)]
rule Tagset{..} x =
  case x `M.lookup` rules of
    Just y  -> y
    Nothing -> error $ "rule: unknown POS " ++ T.unpack x

-- | The morphosyntactic tag consists of the POS value and corresponding
-- attribute values.
data Tag = Tag
    { pos   :: POS
    , atts  :: M.Map Attr AttrVal
    } deriving (Show, Read, Eq, Ord)

-- | Expand optional attributes of the tag.
expand :: Tagset -> Tag -> [Tag]
expand tagset tag = do
    values <- sequence (map attrVal rl)
    let attrMap = M.fromList $ zip (map fst rl) values
    return $ Tag (pos tag) attrMap
  where
    rl = rule tagset (pos tag)
    attrVal (attr, False) = [atts tag M.! attr]
    attrVal (attr, True)
        | Just x <- M.lookup attr (atts tag) = [x]
        | otherwise = S.toList $ domain tagset attr

-- | Measure of similarity between two tags.
tagSim :: Tag -> Tag -> Int
tagSim t t' =
    S.size (xs `S.intersection` xs')
  where
    xs  = S.fromList $ (Nothing, pos t)  : assocs t
    xs' = S.fromList $ (Nothing, pos t') : assocs t'
    assocs = map (first Just) . M.assocs . atts

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

attrSec :: Parser (M.Map Attr (S.Set AttrVal))
attrSec = do
    secName "ATTR" *> spaces
    defs <- attrLine `endBy` spaces
    return $ M.fromList defs

attrLine :: Parser (Attr, S.Set AttrVal)
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
