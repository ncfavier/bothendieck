module Parts.NLab (nLabInit) where

import Control.Monad.IO.Class
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
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
      nlabRandomCommand src args = do
        tags <- liftIO $ fetchTags $ T.unpack $ baseUrl <> "/nlab/list/" <> T.unwords args
        let v = V.fromList (getPages (tagTree tags))
        (name, url) <- pickRandom v
        replyTo src $ ircBold <> name <> ircReset <> " [" <> baseUrl <> url <> "]"
  pure $ M.fromList
    [ ("nlabrandom", nlabRandomCommand)
    ]
