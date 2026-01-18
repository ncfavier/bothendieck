module Parts.Typst (typstInit) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client
import Network.IRC.Client
import System.Process
import System.Exit
import System.IO
import System.IO.Temp

import Utils

typstInit :: Config -> IO (MessageHandler s Text)
typstInit config = do
  let handler (src@Channel{}, False, msg)
        | ("typst", T.stripPrefix ">" -> Just (T.strip . T.replace "↵" "\n" -> input)) <- T.breakOn ">" msg
        , not (T.null input)
        = True <$ do
          withSystemTempFile "typst.pdf" \ pdf handle -> do
            liftIO $ hClose handle
            let p = proc "typst" ["compile", "--format=pdf", "--root=/var/empty", "-", pdf]
            (code, T.pack -> _out, T.pack -> err) <- liftIO $ readCreateProcessWithExitCode p (T.unpack input)
            replyTo src =<< case code of
              ExitSuccess -> ("✓ " <>) <$> (paste config "-.pdf" =<< liftIO (streamFile pdf))
              _           -> ("✗ " <>) <$> paste config "-" (textRequestBody err)
      handler _ = pure False
  pure handler
