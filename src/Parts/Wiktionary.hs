module Parts.Wiktionary where

import Data.Map qualified as M
import Data.Text qualified as T
import Network.IRC.Client

import Parts.URL
import Utils

wiktionaryInit :: IO (Commands s)
wiktionaryInit = do
  let randomWordCommand src args = do
        let (page, anchor) = case args of
              [] -> ("Special:Random", "")
              "-l":(T.toTitle . T.unwords -> lang) ->
                ("Special:RandomInCategory/" <> lang <> " lemmas", "#" <> T.replace " " "_" lang)
              _ -> ("Special:RandomInCategory/" <> T.unwords args, "")
        (mtitle, url) <- fetchUrlTitle ("https://en.wiktionary.org/wiki/" <> page)
        case mtitle of
          Just (T.stripSuffix " - Wiktionary" -> Just title) -> do
            replyTo src (ircBold <> title <> ircReset <> " " <> "[" <> url <> anchor <> "]")
          _ -> pure ()
  pure $ M.fromList
    [("wrandom", randomWordCommand)]
