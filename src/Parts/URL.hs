module Parts.URL (urlTitleInit, urlTitleHandler) where

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
import Network.HTTP.Media
import Network.HTTP.Simple hiding (httpLbs, withResponse)
import Network.IP.Addr
import Network.IRC.Client
import Network.Socket
import Network.URI
import Text.Html.Encoding.Detection
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Regex.TDFA

import Utils

maxUrls :: Int
maxUrls = 3

maxResponseSize :: Int
maxResponseSize = 10 * 1024 * 1024 * 1024

maxTitleLength :: Int
maxTitleLength = 300

urlRegex :: Text
urlRegex = "https?://[^[:space:]]+"

cleanUpURL :: Text -> Text
cleanUpURL = T.dropWhileEnd (`T.elem` ",.:;!?\">")

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

-- | Initialisation action to be called before any requests are made.
urlTitleInit :: IO ()
urlTitleInit = setGlobalManager
           =<< newManager . fst
           =<< mkRestrictedManagerSettings (addressRestriction restrictIPs) Nothing Nothing

fetchUrlTitle :: MonadIO m => Text -> m (Maybe Text)
fetchUrlTitle url = liftIO do
  manager <- getGlobalManager
  request <- addRequestHeader "Accept-Language" "en,*"
           . addRequestHeader "User-Agent" "HendieckBot" -- Twitter is picky about this
           . fixHost
         <$> parseRequestThrow (T.unpack url)
  withResponse request manager \ response -> do
    case mapMaybe (parseAccept @MediaType) $ getResponseHeader "Content-Type" response of
      ct:_ | ct `matches` "text/html"
          || ct `matches` "application/xhtml+xml" -> do
        body <- brReadSome (getResponseBody response) maxResponseSize
        encoding <- maybe (return BE.utf8) BE.mkTextEncoding $ asum
          [ B8.unpack . original <$> ct /. "charset"
          , detectBom body
          , detectMetaCharset . BL.take 1024 $ body
          ]
        let tags = canonicalizeTags . parseTags . BE.decode encoding $ BL.toStrict body
            title = innerText . takeWhile (not . tagCloseNameLit "title") . dropWhile (not . tagOpenNameLit "title") $ tags
        pure $ case T.words title of
          [] -> Nothing
          ws -> Just (T.unwords ws)
      _ -> pure Nothing

-- | Posts the title of URLs contained in messages from public channels.
urlTitleHandler :: EventHandler a
urlTitleHandler = EventHandler matchMessageOrAction \ src msg -> case src of
  Channel _channel _nick -> void $ forkWorker do
    let urls = take maxUrls . map cleanUpURL $ getAllTextMatches (msg =~ urlRegex)
    for_ urls \ url -> fetchUrlTitle url >>= traverse \ title -> do
      replyTo src (ircBold <> "> " <> ircReset <> truncateWithEllipsis maxTitleLength title)
  _ -> pure ()
