{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant &" #-}
module Lib
  ( runApp
  ) where
import           Control.Exception.Base (throw)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Identity (runIdentity)
import           Data.Aeson             (KeyValue((.=)), object)
import           Data.Bifunctor         (Bifunctor(second))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Default           (def)
import           Data.List              (isSuffixOf)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe, isNothing)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Yaml              as Y
import           Data.Yaml.Pretty       as YP (defConfig, encodePretty,
                                               setConfDropNull)
import           Optics                 (Ixed(ix), to, (%), (&), (^.), (^?))
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)
import qualified System.FilePath        as FP
import           Text.DocTemplates      (Context(..), Doc(Text),
                                         ToContext(toContext, toVal),
                                         Val(SimpleVal))
import           Text.Pandoc            (writerTemplate, writerVariables)
import qualified Text.Pandoc            as Pandoc
import qualified Text.Pandoc.PDF        as Pandoc
import           Text.Pandoc.Writers    (writeLaTeX)
import           ToPandoc               (toPandoc)
import           Types                  (BookEntryM,
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
      outpath = args ^? ix 2
      doPdf = isSuffixOf "pdf" <$> outpath

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

  if fromMaybe False doPdf
    then do
      let asPandoc = toPandoc sorted
          options :: Context T.Text = toContext $ object [ "geometry" .= ("margin=1cm" :: T.Text) ]
      -- result <- Pandoc.runIO (Pandoc.writeMarkdown def asPandoc) >>= Pandoc.handleError
      result <- Pandoc.runIO $ do
                template <- Pandoc.compileDefaultTemplate "latex"
                result <- Pandoc.makePDF "xelatex" [] writeLaTeX
                  (def { writerTemplate = Just template
                        , writerVariables = options }) asPandoc
                -- liftIO $ print result
                case result of
                  Left err       -> error $ show err
                  Right content' -> liftIO $ BL.writeFile outfile content'

      case result of
        Left err -> throw err
        Right _  -> TIO.putStrLn "pandoc is cool"
      TIO.putStrLn "pandoc is cool"
    else do
      BS.writeFile outfile (YP.encodePretty config sorted)

  TIO.putStrLn $ "Wrote to: " <> T.pack outfile
  where config = YP.setConfDropNull True YP.defConfig

