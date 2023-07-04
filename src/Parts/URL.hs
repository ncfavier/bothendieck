module Parts.URL (urlTitleInit, fetchUrlTitle) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Encoding qualified as BE
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (original)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder
import Data.Text.IDN.IDNA
import GHC.Generics
import HTMLEntities.Decoder
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client as H
import Network.HTTP.Client.Restricted
import Network.HTTP.Client.TLS
import Network.HTTP.Media hiding ((//))
import Network.HTTP.Simple hiding (httpLbs, withResponse)
import Network.IP.Addr
import Network.IRC.Client hiding (get)
import Network.Socket
import Network.URI
import Text.Html.Encoding.Detection
import Text.HTML.Scalpel hiding (Config, matches)
import Text.Regex.TDFA hiding (empty)

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
fixHostEncoding :: Request -> Request
fixHostEncoding request = setRequestHost host' request where
    host' = either (error . show) id
          . toASCII defaultFlags
          . T.pack . unEscapeString
          . B8.unpack . host
          $ request

-- | Don't propagate cookies sent by the server across requests.
-- This works around Google's cookie consent pages.
noCookies :: Request -> Request
noCookies request = request { cookieJar = Nothing }

-- | Rewrite requests to Twitter to use a JSON API to bypass JavaScript and login walls.
rewriteTwitter :: Request -> Maybe Request
rewriteTwitter request
  | host request `elem` ["twitter.com", "m.twitter.com"]
  , _:"status":statusId:_ <- pathSegments (getUri request)
  = Just
  . setRequestHost (host jsonRequest)
  . setRequestPort (H.port jsonRequest)
  . setRequestSecure (secure jsonRequest)
  . setRequestPath (H.path jsonRequest)
  . setRequestQueryString [ "id" ?= B8.pack statusId ]
  $ request
  | otherwise = Nothing
  where jsonRequest = parseRequestThrow_ "https://cdn.syndication.twimg.com/tweet-result"

-- | Forbids connections to reserved (e.g. local) IP addresses.
restrictIPs :: AddrInfo -> Maybe ConnectionRestricted
restrictIPs AddrInfo{addrAddress = SockAddrInet _ (hostAddressToTuple -> (a, b, c, d))}
  | GeneralIP4 <- ip4Range (ip4FromOctets a b c d) = Nothing
restrictIPs AddrInfo{addrAddress = SockAddrInet6 _ _ (hostAddress6ToTuple -> (a, b, c, d, e, f, g, h)) _}
  | GeneralIP6 <- ip6Range (ip6FromWords a b c d e f g h) = Nothing
restrictIPs _ = Just (ConnectionRestricted "forbidden address")

-- | Disable certificate validation.
tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple True False False

-- | Posts the title of URLs contained in messages from public channels.
urlTitleInit :: IO (MessageHandler Config Text)
urlTitleInit = do
  (managerSettings, _) <- mkRestrictedManagerSettings (addressRestriction restrictIPs) Nothing (Just tlsSettings)
  setGlobalManager =<< newManager managerSettings
  pure \ (src, _action, msg) -> case src of
    Channel _channel _nick -> True <$ do
      let urls = take maxUrls . map cleanUpURL $ getAllTextMatches (msg =~ urlRegex)
      for_ urls \ url -> forkWorker $ runMaybeT do
        (Just title, _) <- fetchUrlTitle url
        lift $ replyTo src $ limitOutput $ ircBold <> "> " <> ircReset <> title
    _ -> pure False

titleScraper :: Scraper Text Text
titleScraper = asum
  [ tweetoid
  , maybeLast =<< chroot "head" (texts $ "title" `atDepth` 1)
  , maybeLast =<< texts "title"
  , meta "title"
  , meta "og:title"
  ] where
    maybeLast [] = empty
    maybeLast l = pure (last l)
    meta prop = attr "content" ("meta" @: ["property" @= prop])
    tweetoid = do
      ogType <- meta "og:type"
      guard (ogType `elem` ["article", "photo", "video", "music"])
      ogTitle <- meta "og:title"
      ogDescription <- meta "og:description"
      pure (ircBold <> ogTitle <> ircReset <> ": " <> ogDescription)

getOriginalUri :: Response a -> Text
getOriginalUri = T.pack . show . getUri . getOriginalRequest

data Tweet = Tweet { text :: Text, user :: TweetUser, entities :: TweetEntities, mediaDetails :: Maybe [TweetEntity] }
  deriving (Generic, FromJSON)
data TweetUser = TweetUser { name :: Text, screen_name :: Text }
  deriving (Generic, FromJSON)
data TweetEntities = TweetEntities { urls :: [TweetEntity] }
  deriving (Generic, FromJSON)
data TweetEntity = TweetEntity { expanded_url :: Text, media_url_https :: Maybe Text, indices :: (Int, Int) }
  deriving (Generic, FromJSON)

-- | Expands shortened URLs and decodes HTML entities in the tweet's text.
getTweetText :: Tweet -> Text
getTweetText Tweet{text, entities, mediaDetails} = LT.toStrict $ toLazyText $ htmlEncodedText $ go spans 0 text
  where
    spans = map (indices.head &&& map (liftA2 fromMaybe expanded_url media_url_https))
          $ groupBy ((==) `on` indices)
          $ sortOn indices
          $ urls entities ++ fromMaybe [] mediaDetails
    go [] _ txt = txt
    go (((a, b), urls):xs) off txt = T.take (a - off) txt <> T.unwords urls <> go xs b (T.drop (b - off) txt)

-- | Fetches the HTML title of a URL and also returns the canonical URL (after performing any redirections).
fetchUrlTitle :: (MonadIO m, MonadState Config m) => Text -> m (Maybe Text, Text)
fetchUrlTitle url = liftIO do
  request <- addRequestHeader "Accept-Language" "en,*"
           . addRequestHeader "User-Agent" "bothendieck (https://github.com/ncfavier/bothendieck)"
           . noCookies
           . fixHostEncoding
         <$> parseRequestThrow (T.unpack url)
  case rewriteTwitter request of
    Just request' -> do
      response <- httpJSON request'
      let t@Tweet { user = TweetUser name screen_name } = getResponseBody response
          title = ircBold <> name <> " (@" <> screen_name <> ")" <> ircReset <> ": " <> T.unwords (T.words (getTweetText t))
      pure (Just title, getOriginalUri response)
    Nothing -> do
      manager <- getGlobalManager
      withResponse request manager \ response -> (,getOriginalUri response) <$> do
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
