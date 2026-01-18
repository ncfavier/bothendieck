module Utils (module Utils, module Data.Function) where

import Conduit (MonadThrow)
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Simple
import Network.IRC.Client hiding (timeout)
import Network.IRC.Conduit (Target)
import System.IO
import System.Random
import System.Timeout
import Toml.Schema.FromValue (FromValue)
import Toml.Schema.Generic

data Config = Config
  { server :: Text
  , tls :: Bool
  , port :: Int
  , nick :: Text
  , password :: Maybe Text
  , realName :: Text
  , channels :: [Text]
  , commandPrefix :: Text
  , pasteUrl :: String
  , pasteField :: Text
  , urlCookieJar :: Maybe Text
  , urlAlternativeHosts :: Map Text Text
  , merriamWebsterKey :: Maybe Text
  , wolframAlphaKey :: Maybe Text
  }
  deriving (Generic)
  deriving FromValue via GenericTomlTable Config

-- | Message handlers are run in sequence on each received message or action until
-- one of them returns True.
type MessageHandler s t = (Source t, Bool, t) -> IRC s Bool

type Command = Source Text -> [Text] -> IRC Config ()
type Commands = Map Text Command

_PrivmsgNoCTCP :: Prism' (Message a) (Target a, a)
_PrivmsgNoCTCP = _Privmsg . aside _Right

matchMessage :: Event Text -> Maybe Text
matchMessage e = snd <$> matchType _PrivmsgNoCTCP e

matchMessageOrAction :: Event Text -> Maybe (Bool, Text)
matchMessageOrAction e =  (False,) <$> matchMessage e
                      <|> (True,) . T.unwords <$> matchCTCP "ACTION" e

workerTimeout :: Int
workerTimeout = 20 -- seconds

forkWorker :: IRC s a -> IRC s ThreadId
forkWorker action = fork do
  s <- getIRCState
  liftIO . handle printExceptions . void . timeout (workerTimeout * 1_000_000) $ runIRCAction action s
  where
    printExceptions :: SomeException -> IO ()
    printExceptions e = do
      print e
      hFlush stdout
      throwIO e

ircBold, ircItalic, ircUnderline, ircReset :: Text
ircBold = T.singleton '\x02'
ircItalic = T.singleton '\x1D'
ircUnderline = T.singleton '\x1F'
ircReset = T.singleton '\x0F'

unwordsOrNone :: [Text] -> Text
unwordsOrNone [] = "(none)"
unwordsOrNone ws = T.unwords ws

truncateWithEllipsis :: Int -> Text -> Text
truncateWithEllipsis n t | T.length t > n = T.take (n - 1) t <> "…"
                         | otherwise = t

limitOutputAt :: Int -> Int -> Text -> Text
limitOutputAt len lin = T.unlines . take lin . T.lines . truncateWithEllipsis len

maxOutputLength :: Int
maxOutputLength = 450

maxOutputLines :: Int
maxOutputLines = 5

limitOutput :: Text -> Text
limitOutput = limitOutputAt maxOutputLength maxOutputLines

(?=) :: a -> b -> (a, Maybe b)
k ?= v = (k, Just v)

pickRandom :: MonadIO m => Vector a -> m a
pickRandom l = (l V.!) <$> randomRIO (0, V.length l - 1)

setCommonRequestParams :: Request -> Request
setCommonRequestParams = addRequestHeader "User-Agent" "bothendieck/0.0 (https://github.com/ncfavier/bothendieck; n+bothendieck@monade.li)"

paste :: (MonadIO m, MonadThrow m) => Config -> FilePath -> Text -> m Text
paste config filename text = do
  request <- setCommonRequestParams <$> parseRequestThrow (pasteUrl config) >>= formDataBody
    [partFileRequestBody (pasteField config) filename $ RequestBodyBS $ T.encodeUtf8 text]
  liftIO $ try (httpBS request) >>= \case
    Left (e :: HttpException) -> "pasting failed" <$ print e
    Right response -> pure $ T.strip . limitOutputAt 256 1 . T.decodeUtf8 $ getResponseBody response
