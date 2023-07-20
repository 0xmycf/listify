module ToPandoc
  ( toPandoc

  , mockPandoc) where

import qualified Data.Map            as M
import qualified Data.Text           as T
import           Optics
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Types

-- toPandoc :: ByT a b -> [BookEntryA] -> Pandoc
toPandoc :: Mapped -> Pandoc
toPandoc entries = doc $
    foldMap go headers
  where headers = ifoldl (\i acc _ -> i : acc) [] entries -- foldl (\acc value -> value ^. #label : acc)
        go h = plain (strong $ str h) <>
                bulletList (toTxt <$> entries M.! h)

mockPandoc :: Mapped -> Pandoc
mockPandoc _ = doc $ plain $ str "Hello World"

toTxt :: BookEntryM -> Blocks
toTxt e = mconcat
            [ plain $ str ("Title: " <> _title e)
            , plain $ str ("Author: " <> _author e)
            , plain $ str "Genres:"
            , bulletList (plain . str <$> e ^. #genres % non [])]
