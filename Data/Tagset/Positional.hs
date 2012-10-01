{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.Tagset.Positional
( Tagset (..)
, Attr
, POS
, Optional
, domain
, rule

, Tag (..)
, expand
, tagSim
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.Binary
import Data.Text.Binary ()
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

-- | Attribute name.
type Attr = T.Text

-- | Part of speech.
type POS = T.Text

-- | Is the attribute optional?
type Optional = Bool

-- | The tagset consists of a domain (set of attribute values) for
-- each attribute name and of a parsing rule for each part of speech.
data Tagset = Tagset
    { domains   :: M.Map Attr (S.Set T.Text)
    , rules     :: M.Map POS  [(Attr, Optional)]
    } deriving (Show)

instance Binary Tagset where
    put ts = put (domains ts)
          >> put (rules ts)
    get = Tagset <$> get <*> get

-- | Set of potential values for the given attribute.
domain :: Tagset -> Attr -> S.Set T.Text
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

-- | A morphosyntactic tag consists of the POS value and corresponding
-- attribute values.
data Tag = Tag
    { pos   :: POS
    , atts  :: M.Map Attr T.Text
    } deriving (Show, Read, Eq, Ord)

instance Binary Tag where
    put Tag{..} = put pos >> put atts
    get = Tag <$> get <*> get

-- | Expand tag optional attributes.
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
