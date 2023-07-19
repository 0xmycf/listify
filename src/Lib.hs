{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant &" #-}
module Lib
  ( runApp
  ) where
import           Control.Monad          (when)
import           Control.Monad.Identity (runIdentity)
import qualified Data.ByteString        as BS
import           Data.Either            (fromRight)
import qualified Data.Map               as M
import           Data.Maybe             (isNothing)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Yaml              as Y
import           Data.Yaml.Pretty       as YP (defConfig, encodePretty,
                                               setConfDropNull)
import           Optics                 (to, (%), (&), (.~), (^.))
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)
import qualified System.FilePath        as FP
import           Types                  (BookEntryM,
                                         BookEntryT(author, genres, title),
                                         ByT(ByAuthor, ByGenre, ByNothing, ByTitle),
                                         entryToMap, mapToEntry)

runApp :: IO ()
runApp = do
  args <- getArgs

  when (length args < 2) do
    TIO.putStrLn "Usage: booklist [author|genre|title] <filepath>"
    exitFailure

  let nfunb l = (& l .~ Nothing)
  let sort = case head args of
              "author" -> ByAuthor ((^. #author % to runIdentity), nfunb #author)
              "genre"  -> ByGenre (^. #genres % to runIdentity)
              "title"  -> ByTitle ((^. #title % to runIdentity), nfunb #title)
              _        -> error "Wrong sort option!"

  let file = args !! 1

  content <- BS.readFile file
  let oldNoSuffix = FP.dropExtension file
      outfile = oldNoSuffix <> "-" <> head args <> "-new.yaml"
      mp = fromRight (error "The map could not get parsed.") $ Y.decodeEither' content :: M.Map T.Text [BookEntryM]
      firstElem = head . snd . head . M.toList $ mp -- if this fails something is wrong anyway
      constructor = if | firstElem ^. #title % to isNothing  -> ByTitle
                       | firstElem ^. #genres % to isNothing -> ByGenre
                       | firstElem ^. #author % to isNothing -> ByAuthor
                       | otherwise                           -> const ByNothing
      intermidiary = mapToEntry $ M.mapKeys constructor mp
      sorted = entryToMap sort intermidiary

  BS.writeFile outfile (YP.encodePretty config sorted)
  TIO.putStrLn $ "Wrote to: " <> T.pack outfile
  where config = YP.setConfDropNull True YP.defConfig

