module Parts.NLab (nLabInit) where

import Control.Monad.IO.Class
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Simple
import Network.IRC.Client
import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.Vector qualified as V

import Utils

getPages :: [TagTree Text] -> [(Text, Text)]
getPages ts = ts >>= ul where
  ul (TagBranch "ul" _ ts) = ts >>= li
  ul (TagBranch _ _ ts) = getPages ts
  ul (TagLeaf _) = []
  li (TagBranch "li" _ ts) = ts >>= a
  li _ = []
  a (TagBranch "a" attrs ts) | Just href <- lookup "href" attrs = [(innerText (flattenTree ts), href)]
  a _ = []

nLabInit :: IO Commands
nLabInit = do
  let baseUrl = "https://ncatlab.org"
      nlabCommand src args = do
        let query = T.unwords args
            request = parseRequestThrow_ (T.unpack $ baseUrl <> "/nlab/search")
                    & setCommonRequestParams
                    & setRequestQueryString [ "query" ?= T.encodeUtf8 query ]
        response <- httpBS request
        let tags = parseTags $ T.decodeUtf8 $ getResponseBody response
            pages = getPages $ tagTree tags
            results = [(n, u) | (n, u) <- pages, n == query]
                   <> [(n, u) | (n, u) <- pages, T.toCaseFold n == T.toCaseFold query]
                   <> pages
        case results of
          (name, url):_ -> replyTo src $ ircBold <> name <> ircReset <> " [" <> baseUrl <> url <> "]"
          _ -> pure ()
      nlabRandomCommand src args = do
        tags <- liftIO $ fetchTags $ T.unpack $ baseUrl <> "/nlab/list/" <> T.unwords args
        let v = V.fromList (getPages (tagTree tags))
        (name, url) <- pickRandom v
        replyTo src $ ircBold <> name <> ircReset <> " [" <> baseUrl <> url <> "]"
  pure $ M.fromList
    [ ("nlab", nlabCommand)
    , ("nlabrandom", nlabRandomCommand)
    ]
