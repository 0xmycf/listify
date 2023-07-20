{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant &" #-}
module Lib
  ( runApp
  ) where
import           Control.Exception         (Exception(displayException))
import           Control.Exception.Base    (throw)
import           Control.Monad             (when, (>=>))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Identity    (runIdentity)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           Data.Default              (def)
import           Data.Either               (fromRight)
import           Data.List                 (isPrefixOf, isSuffixOf)
import           Data.List.Split           (endsWith)
import qualified Data.List.Split           as C
import qualified Data.Map                  as M
import           Data.Maybe                (fromMaybe, isNothing)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Yaml                 as Y
import           Data.Yaml.Pretty          as YP (defConfig, encodePretty,
                                                  setConfDropNull)
import           Optics                    (Ixed(ix), to, (%), (&), (^.), (^?))
import           System.Environment        (getArgs)
import           System.Exit               (exitFailure)
import qualified System.FilePath           as FP
import           Text.Pandoc               (writerTemplate)
import qualified Text.Pandoc               as Pandoc
import           Text.Pandoc.App           (Opt, optPdfEngineOpts)
import qualified Text.Pandoc.PDF           as Pandoc
import           Text.Pandoc.Writers.LaTeX (writeLaTeX)
import           ToPandoc                  (mockPandoc, toPandoc)
import           Types                     (BookEntryM,
                                            BookEntryT(author, genres, title),
                                            ByT(ByAuthor, ByGenre, ByNothing, ByTitle),
                                            entryToMap, mapToEntry)

runApp :: IO ()
runApp = do
  args <- getArgs

  when (length args < 2) do
    TIO.putStrLn "Usage: booklist [author|genre|title] <filepath>.[json|yaml] {<filepath-out>.[yaml|pdf]} "
    exitFailure

  let sort = case head args of
              "author" -> ByAuthor (^. #author % to runIdentity)
              "genre"  -> ByGenre (^. #genres % to runIdentity)
              "title"  -> ByTitle (^. #title % to runIdentity)
              _        -> error "Sort options are: [author|genre|title]"

  let file = args !! 1

  content <- BS.readFile file
  let oldNoSuffix = FP.dropExtension file
      outfile = fromMaybe (oldNoSuffix <> "-" <> head args <> ".yaml") outpath
      mp = case Y.decodeEither' content  of
              Right mp_ -> mp_ :: M.Map T.Text [BookEntryM]
              Left err  -> throw err
      constructor key =
        let value = mp M.! key
            firstElem = head value
        in key & if | firstElem ^. #title % to isNothing  -> ByTitle
                    | firstElem ^. #author % to isNothing -> ByAuthor
                    | firstElem ^. #genres % to isNothing -> ByGenre
                    | otherwise                           -> ByNothing

      intermidiary = mapToEntry $ M.mapKeys constructor mp
      sorted = entryToMap sort intermidiary

  BS.writeFile outfile (YP.encodePretty config sorted)
  TIO.putStrLn $ "Wrote to: " <> T.pack outfile
  where config = YP.setConfDropNull True YP.defConfig

