module Parts.Wikimedia (wikimediaInit) where

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.IRC.Client
import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree

import Parts.URL
import Utils

wikimediaInit :: IO Commands
wikimediaInit = do
  let randomPageCommand baseUrl src args = do
        let (page, anchor) = case args of
              [] -> ("Special:Random", "")
              "-l":(T.toTitle . T.unwords -> lang) -> -- convenience for Wiktionary
                ("Special:RandomInCategory/" <> lang <> " lemmas", "#" <> T.replace " " "_" lang)
              _ -> ("Special:RandomInCategory/" <> T.unwords args, "")
        (mtitle, url) <- fetchUrlTitle (baseUrl <> "/wiki/" <> page)
        case mtitle of
          Just (T.breakOn " - " -> (title, _)) -> do
            replyTo src (ircBold <> title <> ircReset <> " " <> "[" <> url <> anchor <> "]")
          _ -> pure ()

      summaryCommand baseUrl src [] = randomPageCommand baseUrl src []
      summaryCommand baseUrl src args = do
        let request = parseRequestThrow_ (T.unpack baseUrl <> "/wiki")
                    & setCommonRequestParams
                    & setRequestQueryString [ "search" ?= T.encodeUtf8 (T.unwords args) ]
        response <- httpBS request
        let scraper = text $ "div" @: [hasClass "mw-parser-output"] // "p" @: [notP $ hasClass "mw-empty-elt"] `atDepth` 1
            summary = scrape scraper . flattenTree . transformTree f . tagTree . parseTags . T.decodeUtf8 $ getResponseBody response
            f (TagBranch "style" _ _) = []
            f (TagBranch _ attr _) | anyAttrLit ("encoding", "application/x-tex") attr
                                  || "reference" `elem` classes attr = []
            f x = [x]
            classes attr = [c | ("class", cs) <- attr, c <- T.words cs]
            url = T.pack . show . getUri $ getOriginalRequest response
        replyTo src (maybe "" (limitOutput . T.unwords . T.words) summary <> "[" <> url <> "]")

      wikipediaRandomCommand = randomPageCommand "https://en.wikipedia.org"
      wikipediaSummaryCommand = summaryCommand "https://en.wikipedia.org"
      wiktionaryRandomCommand = randomPageCommand "https://en.wiktionary.org"
      wiktionarySummaryCommand = summaryCommand "https://en.wiktionary.org"
  pure $ M.fromList
    [ ("wp", wikipediaSummaryCommand)
    , ("wprandom", wikipediaRandomCommand)
    , ("wt", wiktionarySummaryCommand)
    , ("wʃ", wiktionarySummaryCommand)
    , ("wtrandom", wiktionaryRandomCommand)
    , ("wʃrandom", wiktionaryRandomCommand)
    ]
