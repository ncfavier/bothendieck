module Parts.URL (urlTitleInit, fetchUrlTitle) where

import Control.Applicative
import Control.Concurrent.ParallelIO.Local
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Encoding qualified as BE
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (original)
import Data.Either
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.IDN.IDNA
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

data Link = URL Text | Wikilink Text deriving (Show)

maxLinks :: Int
maxLinks = 3

maxResponseSize :: Int
maxResponseSize = 10 * 1024 * 1024 * 1024

linkRegex :: Text
linkRegex = "https?://[^[:space:]]+|\\[\\[[^][]+\\]\\]"

-- TODO do this properly...
cleanUpURL :: Text -> Link
cleanUpURL (T.stripPrefix "[[" -> Just (T.stripSuffix "]]" -> Just link)) = Wikilink link
cleanUpURL url = URL $ T.dropWhileEnd (`T.elem` (",.:;!?\">" <> if '(' `T.elem` url then "" else ")")) url

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

{-
data Tweet = Tweet { text :: Text, user :: TweetUser, entities :: TweetEntities, mediaDetails :: Maybe [TweetEntity] }
  deriving (Generic, FromJSON)
data TweetUser = TweetUser { name :: Text, screen_name :: Text }
  deriving (Generic, FromJSON)
data TweetEntities = TweetEntities { urls :: [TweetEntity] }
  deriving (Generic, FromJSON)
data TweetEntity = TweetEntity { expanded_url :: Text, media_url_https :: Maybe Text, indices :: (Int, Int), ext_alt_text :: Maybe Text }
  deriving (Generic, FromJSON)

-- | Expands shortened URLs and decodes HTML entities in the tweet's text.
getTweetText :: Tweet -> Text
getTweetText Tweet{..} = LT.toStrict $ toLazyText $ htmlEncodedText $ go spans 0 text
  where
    spans = map (indices.head &&& map entityUrl)
          $ groupBy ((==) `on` indices)
          $ sortOn indices
          $ urls entities ++ fromMaybe [] mediaDetails
    entityUrl TweetEntity{..} = fromMaybe expanded_url media_url_https <> maybe "" (\ a -> " [" <> T.strip a <> "]") ext_alt_text
    go [] _ txt = txt
    go (((a, b), urls):xs) off txt = T.take (a - off) txt <> T.unwords urls <> go xs b (T.drop (b - off) txt)
-}

-- | Treat some requests specially.
special :: Request -> Maybe (IO (Maybe Text, Text))
{-
special request
  | host request `elem` ["twitter.com", "m.twitter.com"]
  , _:"status":statusId:_ <- pathSegments (getUri request)
  = Just do
    let request' = setRequestHost (host jsonRequest)
                 . setRequestPort (H.port jsonRequest)
                 . setRequestSecure (secure jsonRequest)
                 . setRequestPath (H.path jsonRequest)
                 . setRequestQueryString [ "id" ?= B8.pack statusId ]
                 $ request
    response <- httpJSON request'
    let t@Tweet { user = TweetUser name screen_name } = getResponseBody response
        title = ircBold <> name <> " (@" <> screen_name <> ")" <> ircReset <> ": " <> T.unwords (T.words (getTweetText t))
    pure (Just title, getOriginalUri response)
  where jsonRequest = parseRequestThrow_ "https://cdn.syndication.twimg.com/tweet-result" -- does not work any longer
-}
special _ = Nothing

rewriteHost :: Map Text Text -> Request -> Request
rewriteHost alt request
  | Just a <- alt Map.!? T.decodeLatin1 (host request)
  , let altRequest = parseRequest_ (T.unpack a)
  = setRequestHost (host altRequest)
  . setRequestPort (H.port altRequest)
  . setRequestSecure (secure altRequest)
  $ request
  | otherwise = request

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
      let links = take maxLinks . map cleanUpURL $ getAllTextMatches (msg =~ linkRegex)
      unless (null links) do
        s <- getIRCState
        (errs, titles) <- liftIO $ partitionEithers <$> withPool (length links) \ pool -> parallelE pool [runIRCAction (processLink l) s | l <- links]
        sequence_ [replyTo src $ limitOutput $ ircBold <> "> " <> ircReset <> title | (Just title, _) <- titles]
        liftIO $ mapM_ print errs
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

processLink :: (MonadIO m, MonadState Config m) => Link -> m (Maybe Text, Text)
processLink (URL url) = fetchUrlTitle url
processLink (Wikilink link) = do
  (t, url') <- fetchUrlTitle url
  pure (t <&> \t -> fromMaybe t (T.stripSuffix " - Wikipedia" t) <> " [" <> url' <> "]", url')
  where url = "https://en.wikipedia.org/wiki/" <> link

-- | Fetches the HTML title of a URL and also returns the canonical URL (after performing any redirections).
fetchUrlTitle :: (MonadIO m, MonadState Config m) => Text -> m (Maybe Text, Text)
fetchUrlTitle url = get >>= \ config -> liftIO do
  request <- addRequestHeader "Accept-Language" "en,*"
           . addRequestHeader "User-Agent" "bothendieck (https://github.com/ncfavier/bothendieck)"
           . noCookies
           . rewriteHost (urlAlternativeHosts config)
           . fixHostEncoding
         <$> parseRequestThrow (T.unpack url)
  flip fromMaybe (special request) do
    manager <- getGlobalManager
    withResponse request manager \ response -> (,getOriginalUri response) <$> do
      case mapMaybe (parseAccept @MediaType) $ getResponseHeader "Content-Type" response of
        ct:_ | ct `matches` "text/html"
            || ct `matches` "application/xhtml+xml"
            || ct `matches` "application/xml" -> do
          body <- brReadSome (getResponseBody response) maxResponseSize
          encoding <- traverse BE.mkTextEncoding $ asum
            [ B8.unpack . original <$> ct /. "charset"
            , detectBom body
            , detectMetaCharset . BL.take 1024 $ body
            -- skip detectEncodingName as it tends to incorrectly guess ASCII
            ]
          let decode = maybe (T.decodeUtf8With T.lenientDecode) BE.decode encoding
              title = scrapeStringLike (decode $ BL.toStrict body) titleScraper
          pure $ T.unwords . T.words <$> title
        _ -> pure Nothing
