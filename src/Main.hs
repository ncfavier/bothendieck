module Main where

import Control.Lens
import Control.Monad.Extra
import Data.Foldable
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding
import GHC.Generics
import Network.IRC.Client as IRC hiding (server, port, nick, password)
import Options.Applicative hiding (action)
import Toml qualified

import Parts.Compliment
import Parts.Eval
import Parts.MerriamWebster
import Parts.Translate
import Parts.URL
import Parts.WolframAlpha
import Parts.Wikimedia
import Utils

data Options = Options
  { configFile :: FilePath
  , extraConfigFiles :: [FilePath]
  , verbose :: Bool
  }

data Config = Config
  { server :: Text
  , tls :: Bool
  , port :: Int
  , nick :: Text
  , password :: Maybe Text
  , realName :: Text
  , channels :: [Text]
  , commandPrefix :: Text
  , merriamWebsterKey :: Maybe Text
  , wolframAlphaKey :: Maybe Text
  } deriving (Generic)

parseOptions :: Parser Options
parseOptions = do
  configFile <- strOption (long "config" <> short 'c' <> metavar "PATH" <> help "Configuration file to use" <> value "config.toml" <> showDefault)
  extraConfigFiles <- many $ strOption (long "extra-config" <> short 'C' <> metavar "PATH" <> help "Extra configuration files")
  verbose <- switch (long "verbose" <> short 'v' <> help "Log every IRC message to standard output")
  pure Options{..}

main :: IO ()
main = do
  options <- execParser (info (parseOptions <**> helper) mempty)
  toml <- T.unlines <$> traverse T.readFile ([configFile options] <> extraConfigFiles options)
  let config = either (error . T.unpack . Toml.prettyTomlDecodeErrors) id $ Toml.decodeExact Toml.genericCodec toml
  complimentCommands <- complimentInit
  (evalHandler, evalCommands) <- evalInit
  merriamWebsterCommands <- merriamWebsterInit (merriamWebsterKey config)
  translateCommands <- translateInit
  urlTitleHandler <- urlTitleInit
  wolframAlphaCommands <- wolframAlphaInit (wolframAlphaKey config)
  wikimediaCommands <- wikimediaInit
  let getConnection h p
        | tls config = tlsConnection (WithDefaultConfig h p)
        | otherwise = plainConnection h p
      authenticate pass = send $ Privmsg "NickServ" $ Right $ T.unwords ["IDENTIFY", nick config, pass]
      commands = mconcat [complimentCommands, evalCommands, merriamWebsterCommands, translateCommands, wolframAlphaCommands, wikimediaCommands]
      commandHandler (src@Channel{}, False, msg)
        | Just (cmd:args) <- T.words <$> T.stripPrefix (commandPrefix config) msg
        , Just runCommand <- commands M.!? cmd
        = True <$ runCommand src args
      commandHandler _ = pure False
      messageHandlers = [commandHandler, evalHandler, urlTitleHandler]
      handleMessages = EventHandler matchMessageOrAction \ src (action, msg) -> do
        void . forkWorker $ anyM ($ (src, action, msg)) messageHandlers
      conn = getConnection (encodeUtf8 (server config)) (fromIntegral (port config))
           & username .~ nick config
           & realname .~ realName config
           & onconnect .~ (defaultOnConnect >> for_ (password config) authenticate)
           & logfunc .~ (if verbose options then stdoutLogger else noopLogger)
      cfg  = defaultInstanceConfig (nick config)
           & IRC.channels .~ Main.channels config
           & handlers <>~ [handleMessages]
  runClient conn cfg ()
