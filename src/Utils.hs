module Utils where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as T
import Network.IRC.Client hiding (timeout)
import Network.IRC.Conduit (Target)
import System.Timeout

_PrivmsgNoCTCP :: Prism' (Message a) (Target a, a)
_PrivmsgNoCTCP = _Privmsg . aside _Right

matchMessage :: Event Text -> Maybe Text
matchMessage e = snd <$> matchType _PrivmsgNoCTCP e

matchMessageOrAction :: Event Text -> Maybe Text
matchMessageOrAction e = matchMessage e <|> T.unwords <$> matchCTCP "ACTION" e

forkWorker :: IRC s () -> IRC s ThreadId
forkWorker action = fork do
  s <- getIRCState
  void . liftIO . timeout 15_000_000 $ runIRCAction action s

ircBold, ircReset :: Text
ircBold = T.singleton '\x02'
ircReset = T.singleton '\x0F'

truncateWithEllipsis :: Int -> Text -> Text
truncateWithEllipsis n t | T.length t > n = T.take (n - 1) t <> "…"
                         | otherwise = t
