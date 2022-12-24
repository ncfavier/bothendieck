module Parts.URL (urlTitleInit, fetchUrlTitle) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Encoding qualified as BE
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (original)
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IDN.IDNA
import Network.HTTP.Client
import Network.HTTP.Client.Restricted
import Network.HTTP.Client.TLS
import Network.HTTP.Media hiding ((//))
import Network.HTTP.Simple hiding (httpLbs, withResponse)
import Network.IP.Addr
import Network.IRC.Client
import Network.Socket
import Network.URI
import Text.Html.Encoding.Detection
import Text.HTML.Scalpel hiding (matches)
import Text.Regex.TDFA

import Utils

maxUrls :: Int
maxUrls = 3

maxResponseSize :: Int
maxResponseSize = 10 * 1024 * 1024 * 1024

urlRegex :: Text
urlRegex = "https?://[^[:space:]]+"

-- TODO do this properly...
cleanUpURL :: Text -> Text
cleanUpURL url = T.dropWhileEnd (`T.elem` (",.:;!?\">" <> if '(' `T.elem` url then "" else ")")) url

-- | Fixes the encoding of a Unicode hostname from percent-encoding (as a result of `escapeURIString`) to IDNA.
fixHost :: Request -> Request
fixHost request = setRequestHost host' request where
    host' = either (error . show) id
          . toASCII defaultFlags
          . T.pack . unEscapeString
          . B8.unpack . host
          $ request

-- | Forbids connections to reserved (e.g. local) IP addresses.
restrictIPs :: AddrInfo -> Maybe ConnectionRestricted
restrictIPs AddrInfo{addrAddress = SockAddrInet _ (hostAddressToTuple -> (a, b, c, d))} =
  case ip4Range (ip4FromOctets a b c d) of
    GeneralIP4 -> Nothing
    _ -> Just (ConnectionRestricted "forbidden IPv4")
restrictIPs AddrInfo{addrAddress = SockAddrInet6 _ _ (hostAddress6ToTuple -> (a, b, c, d, e, f, g, h)) _} =
  case ip6Range (ip6FromWords a b c d e f g h) of
    GeneralIP6 -> Nothing
    _ -> Just (ConnectionRestricted "forbidden IPv6")
restrictIPs _ = Just (ConnectionRestricted "forbidden address")

-- | Posts the title of URLs contained in messages from public channels.
urlTitleInit :: IO (MessageHandler s Text)
urlTitleInit = do
  setGlobalManager =<< newManager . fst =<< mkRestrictedManagerSettings (addressRestriction restrictIPs) Nothing Nothing
  pure \ (src, _action, msg) -> case src of
    Channel _channel _nick -> True <$ do
      let urls = take maxUrls . map cleanUpURL $ getAllTextMatches (msg =~ urlRegex)
      for_ urls \ url -> fst <$> fetchUrlTitle url >>= traverse \ title -> do
        replyTo src $ limitOutput $ ircBold <> "> " <> ircReset <> title
    _ -> pure False

titleScraper :: Scraper Text Text
titleScraper = asum
  [ article -- for Tweet-like things
  , last <$> texts ("head" // "title")
  , last <$> texts "title"
  , meta "title"
  , meta "og:title"
  ] where
    meta prop = attr "content" ("meta" @: ["property" @= prop])
    article = do
      ogType <- meta "og:type"
      ogTitle <- meta "og:title"
      ogDescription <- meta "og:description"
      (ogTitle <> ": " <> ogDescription) <$ guard (ogType == "article")

-- | Fetches the HTML title of a URL and also returns the canonical URL (after performing any redirections).
fetchUrlTitle :: MonadIO m => Text -> m (Maybe Text, Text)
fetchUrlTitle url = liftIO do
  request <- addRequestHeader "Accept-Language" "en,*"
           . addRequestHeader "User-Agent" "SomeHaskellBot" -- Twitter is picky about this
           . fixHost
         <$> parseRequestThrow (T.unpack url)
  manager <- getGlobalManager
  withResponse request manager \ response -> (,T.pack . show . getUri $ getOriginalRequest response) <$> do
    case mapMaybe (parseAccept @MediaType) $ getResponseHeader "Content-Type" response of
      ct:_ | ct `matches` "text/html"
          || ct `matches` "application/xhtml+xml" ->  do
        body <- brReadSome (getResponseBody response) maxResponseSize
        encoding <- maybe (return BE.utf8) BE.mkTextEncoding $ asum
          [ B8.unpack . original <$> ct /. "charset"
          , detectBom body
          , detectMetaCharset . BL.take 1024 $ body
          ]
        let title = scrapeStringLike (BE.decode encoding $ BL.toStrict body) titleScraper
        pure $ T.unwords . T.words <$> title
      _ -> pure Nothing
