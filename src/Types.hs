{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant &" #-}
module Types
    ( ByT(..)
    , By
    , BookEntryT(..)
    , BookEntryA
    , BookEntryM
    , entryToMap
    , mapToEntry
    ) where

import           Control.Monad         (guard, join)
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Bifunctor        (Bifunctor(first))
import           Data.Functor.Identity (Identity(..))
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import qualified Data.Set              as S
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           Optics                (At(at), Each(each), non, to, (%), (%~),
                                        (&), (.~), (^.))

data ByT a b
  = ByAuthor a
  | ByGenre b
  | ByTitle a
  | ByNothing
  deriving (Eq, Ord)

type By = ByT T.Text T.Text

data BookEntryT f
  = BookEntry
      { isbn      :: f (Maybe T.Text)
      , title     :: f T.Text
      , rating    :: f (Maybe Float)
      , hasRead   :: f Bool
      , hasBought :: f Bool
      , author    :: f T.Text
      , genres    :: f [T.Text]
      }
  deriving (Generic)

type BookEntryM = BookEntryT Maybe
instance ToJSON BookEntryM
instance FromJSON BookEntryM
deriving instance Eq BookEntryM
deriving instance Show BookEntryM

type BookEntryA = BookEntryT Identity
instance ToJSON BookEntryA
instance FromJSON BookEntryA
deriving instance Eq BookEntryA
deriving instance Ord BookEntryA

mToA :: BookEntryM -> BookEntryA
mToA BookEntry{..} = BookEntry
                      { isbn      = pure $ join isbn
                      , title     = pure $ fromMaybe "No Title" title
                      , rating    = pure $ join rating
                      , hasRead   = pure $ fromMaybe False hasRead
                      , hasBought = pure $ fromMaybe False hasRead
                      , author    = pure $ fromMaybe "No Author" author
                      , genres    = pure $ fromMaybe [] genres
                      }

aToM :: (BookEntryM -> BookEntryM)
        -> BookEntryA
        -> BookEntryM
aToM fun BookEntry{..} = fun BookEntry
                      { isbn      = fmapI isbn
                      , title     = dotI title
                      , rating    = fmapI rating
                      , hasRead   = dotI hasRead
                      , hasBought = dotI hasBought
                      , author    = dotI author
                      , genres    = dotI genres
                      }
  where fmapI = Just <$> runIdentity
        dotI  = Just . runIdentity

_author, _title :: BookEntryM -> T.Text
_author = (^. #author % non "No Author")
_title = (^. #title % non "No Title")

mapToEntry :: M.Map By [BookEntryM] -> [BookEntryA]
mapToEntry mp = S.toList $ M.foldrWithKey' go S.empty mp
  where go :: By -> [BookEntryM] -> S.Set BookEntryA -> S.Set BookEntryA
        go key value acc =
          let avalues = mToA <$> value
          in case key of
              ByAuthor author -> S.fromList (avalues & each % #author .~ pure author) `S.union` acc
              ByGenre genre   -> S.fromList
                (avalues & each %~ \(be :: BookEntryA) ->
                    let (title, author) = (be ^. #title % to runIdentity, be ^. #author % to runIdentity)
                        allGenres = S.fromList do
                          (ByGenre innerGenre, entries) <- M.toList mp
                          guard (innerGenre /= genre)
                          entry <- entries
                          guard (title == _title entry && author == _author entry)
                          pure innerGenre
                    in (be & #genres .~ pure (S.toList $ genre `S.insert` allGenres))
                ) `S.union` acc

              ByTitle title   -> S.fromList (avalues & each % #title .~ pure title) `S.union` acc
              ByNothing       -> S.fromList avalues `S.union` acc

entryToMap :: Foldable t
              => ByT (BookEntryA -> T.Text, BookEntryM -> BookEntryM) (BookEntryA -> [T.Text])
              -> t BookEntryA
              -> M.Map T.Text [BookEntryM]
entryToMap by' = foldr go M.empty
  where (getter, fun) = case by' of
              ByAuthor author -> first Right author
              ByGenre genre   -> (Left genre, \e -> e & #genres .~ Nothing)
              ByTitle title   -> first Right title
              ByNothing       -> first Right (const "Sorted by Nothing", id)
        go :: BookEntryA -> M.Map T.Text [BookEntryM] -> M.Map T.Text [BookEntryM]
        go entry mp = case getter of
          Right get -> mp & at (get entry) % non [] %~ (aToM fun entry:)
          Left  get ->
            let genres = get entry
            in foldr (\genre accmp -> accmp & at genre % non [] %~ (\ls ->
              let inner = aToM fun entry
              in if inner `elem` ls
                  then ls
                  else inner : ls
                )) mp genres
