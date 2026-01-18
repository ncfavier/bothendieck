module Parts.Typst (typstInit) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as T
import Network.IRC.Client
import System.Process
import System.Exit

import Utils

typstInit :: Config -> IO (MessageHandler s Text)
typstInit config = do
  let handler (src@Channel{}, False, msg)
        | ("typst", T.stripPrefix ">" -> Just (T.strip . T.replace "↵" "\n" -> input)) <- T.breakOn ">" msg
        , not (T.null input)
        = True <$ do
          let p = proc "typst" ["compile", "--features=html", "--format=html", "--root=/var/empty", "-", "-"]
          (code, T.pack -> out, T.pack -> err) <- liftIO $ readCreateProcessWithExitCode p (T.unpack input)
          replyTo src =<< case code of
            ExitSuccess -> ("✓ " <>) <$> paste config "-.html" out
            _           -> ("✗ " <>) <$> paste config "-" err
      handler _ = pure False
  pure handler
