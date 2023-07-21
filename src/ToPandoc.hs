module ToPandoc
  ( toPandoc
  , createPDF
  , createMarkdown) where

import           Control.Exception      (throw)
import           Control.Monad          (join, (>=>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Char              as C
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Optics                 (A_Getter, FoldableWithIndex(ifoldl),
                                         Is, Ixed(ix), Optic', to, (%), (%~),
                                         (&), (^.))
import           Text.DocTemplates
import           Text.Pandoc            (Pandoc,
                                         WriterOptions(writerTemplate, writerVariables),
                                         def, writeLaTeX)
import qualified Text.Pandoc            as Pandoc
import           Text.Pandoc.Builder    (Blocks, bulletList, doc, headerWith,
                                         plain, str)
import qualified Text.Pandoc.PDF        as PandocPDF
import           Types                  (BookEntryM,
                                         BookEntryT(author, genres, hasBought, hasRead, isbn, notes, rating, title),
                                         Mapped)

toPandoc :: Mapped -> Pandoc
toPandoc entries = doc $
    foldMap go headers
  where headers = ifoldl (\i acc _ -> i : acc) [] entries
        go h = headerWith ("",["unnumbered"],[]) 2 (str h) <>
                bulletList (toTxt <$> entries M.! h)

toTxt :: BookEntryM -> Blocks
toTxt e = mconcat
            [ author
            , title
            , rating
            , hasRead
            , hasBought
            , isbn
            , notes
            , genres
            ]
  where author = memptyOrBlocksDef e #author "Author"
        title = memptyOrBlocksDef e #title "Title"
        genres = memptyOrBlocks e #genres (\x -> plain (str "Genres:") <>
                                          bulletList (plain . str <$> x))
        isbn = memptyOrBlocksDef e (#isbn % to join) "ISBN"
        rating = memptyOrBlocksDef e (#rating % to (fmap (T.pack . show) . join)) "Rating"
        hasRead = memptyOrBlocksDef e (#hasRead % to (fmap $ T.pack . show)) "Read:"
        hasBought = memptyOrBlocksDef e (#hasBought % to (fmap $ T.pack . show)) "Bought"
        notes = memptyOrBlocksDef e (#notes % to (fmap ("\n\n"<>) . join)) "Notes"


memptyOrBlocks :: (Monoid b, Is k A_Getter)
               => s
               -> Optic' k is s (Maybe a)
               -> (a -> b)
               -> b
memptyOrBlocks e label blocks = maybe mempty blocks (e ^. label)

memptyOrBlocksDef :: Is k A_Getter
                  => s
                  -> Optic' k is s (Maybe T.Text)
                  -> T.Text
                  -> Blocks
memptyOrBlocksDef e label s =  case e ^. label of
                                Nothing -> mempty
                                Just t  -> plain $ str (s <> ": " <> t)

createMarkdown :: FilePath -> Mapped -> IO ()
createMarkdown fp = (Pandoc.runIO . Pandoc.writeMarkdown def . toPandoc)
                  >=> Pandoc.handleError
                  >=> T.writeFile fp

createPDF :: FilePath -- ^ the path to the outputfile
          -> Mapped
          -> String -- ^ head args (the sort option)
          -> FilePath -- ^ the path to the template
          -> IO ()
createPDF outfile sorted sortString templatePath = do
      let asPandoc = toPandoc sorted
          options :: Context T.Text = toContext $
            object [ "sortby" .= (T.pack $ sortString & ix 0 %~ C.toUpper :: T.Text) ]
      result <- Pandoc.runIO $ do
                tempres <- Pandoc.getTemplate templatePath -- "./template.tex"
                tempres2 <- liftIO $ Pandoc.compileTemplate "" tempres
                let template = case tempres2 of
                                Left err   -> error . show $ err
                                Right temp -> temp
                result <- PandocPDF.makePDF "xelatex" [] writeLaTeX
                  (def { writerTemplate = Just template
                        , writerVariables = options }) asPandoc
                case result of
                  Left err       -> error $ show err
                  Right content' -> liftIO $ BL.writeFile outfile content'
      case result of
        Left err -> throw err
        Right _  -> pure ()
