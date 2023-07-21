{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant &" #-}
module Lib
  ( runApp
  ) where
import           Control.Exception.Base (throw)
import           Control.Monad          (unless, when)
import           Control.Monad.Identity (runIdentity)
import qualified Data.ByteString        as BS
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe, isNothing)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Yaml              as Y
import           Data.Yaml.Pretty       as YP (defConfig, encodePretty,
                                               setConfDropNull)
import           Network.Curl           (CurlCode(CurlOK), curlGetString,
                                         withCurlDo)
import           Optics                 (Ixed(ix), to, (%), (&), (^.), (^?))
import qualified System.Directory       as SD
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)
import           System.FilePath
import qualified System.FilePath        as FP
import           ToPandoc               (createMarkdown, createPDF)
import           Types                  (BookEntryM,
                                         BookEntryT(author, genres, title),
                                         ByT(ByAuthor, ByGenre, ByNothing, ByTitle),
                                         entryToMap, mapToEntry)

runApp :: IO ()
runApp = do
  args <- getArgs

  when (length args < 2) do
    TIO.putStrLn "Usage: booklist [author|genre|title] <filepath>.[json|yaml] {<filepath-out>.[yaml|pdf|md]} "
    exitFailure

  let sort = case head args of
              "author" -> ByAuthor (^. #author % to runIdentity)
              "genre"  -> ByGenre (^. #genres % to runIdentity)
              "title"  -> ByTitle (^. #title % to runIdentity)
              _        -> error "Sort options are: [author|genre|title]"
      file = args !! 1
      outpath = args ^? ix 2
      extension = FP.takeExtension <$> outpath

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

  path <- SD.getXdgDirectory SD.XdgCache "listify"
  SD.createDirectoryIfMissing False path
  let templatePath = path </> "template.tex"
  doesTemplatePathExist <- SD.doesFileExist templatePath
  unless doesTemplatePathExist $
    withCurlDo $ do
      (code, getResult) <- curlGetString "https://raw.githubusercontent.com/0xmycf/listify/main/template.tex" []
      case code of
        CurlOK -> writeFile templatePath getResult
        _      -> error "Curl Error"

  case maybe "else" (drop 1) extension of
    "pdf" -> createPDF outfile sorted (head args) templatePath
    "md"  -> createMarkdown outfile sorted
    _     -> BS.writeFile outfile (YP.encodePretty config sorted)

  TIO.putStrLn $ "Wrote to: " <> T.pack outfile
  where config = YP.setConfDropNull True YP.defConfig

