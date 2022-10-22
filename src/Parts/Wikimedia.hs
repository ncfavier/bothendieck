module Parts.Wikimedia where

import Data.Map qualified as M
import Data.Text qualified as T
import Network.IRC.Client

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
  pure $ M.fromList
    [ ("wprandom", wikipediaRandomCommand)
    , ("wtrandom", wiktionaryRandomCommand)
    , ("wʃrandom", wiktionaryRandomCommand)
    ]
