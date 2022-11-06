module Parts.Wikimedia where

import Control.Monad.Extra
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.IRC.Client
import Text.HTML.Scalpel
import Text.HTML.TagSoup

import Parts.URL
import Utils

wikimediaInit :: IO (Commands s)
wikimediaInit = do
  let randomPageCommand project baseUrl src args = do
        let (page, anchor) = case args of
              [] -> ("Special:Random", "")
              "-l":(T.toTitle . T.unwords -> lang) -> -- convenience for Wiktionary
                ("Special:RandomInCategory/" <> lang <> " lemmas", "#" <> T.replace " " "_" lang)
              _ -> ("Special:RandomInCategory/" <> T.unwords args, "")
        (mtitle, url) <- fetchUrlTitle (baseUrl <> "/wiki/" <> page)
        case mtitle of
          Just (T.stripSuffix (" - " <> project) -> Just title) -> do
            replyTo src (ircBold <> title <> ircReset <> " " <> "[" <> url <> anchor <> "]")
          _ -> pure ()
      wikipediaRandomCommand = randomPageCommand "Wikipedia" "https://en.wikipedia.org"
      wiktionaryRandomCommand = randomPageCommand "Wiktionary" "https://en.wiktionary.org"
      wikipediaSummaryCommand src args = do
        response <- httpBS =<< parseRequestThrow ("https://en.wikipedia.org/wiki/" <> T.unpack (T.unwords args))
        let scraper = text $ "div" @: [hasClass "mw-parser-output"] // "p" @: [notP $ hasClass "mw-empty-elt"] `atDepth` 1
            summary = scrape scraper . parseTags . T.decodeUtf8 $ getResponseBody response
            url = T.pack . show . getUri $ getOriginalRequest response
        whenJust summary \ s -> do
          replyTo src (truncateWithEllipsis 400 s)
        replyTo src ("[" <> url <> "]")
  pure $ M.fromList
    [ ("wp", wikipediaSummaryCommand)
    , ("wprandom", wikipediaRandomCommand)
    , ("wtrandom", wiktionaryRandomCommand)
    , ("wʃrandom", wiktionaryRandomCommand)
    ]
