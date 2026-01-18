module Main where

import Control.Lens
import Control.Monad.Extra
import Data.Foldable
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO as T
import Data.Text.Encoding
import Network.IRC.Client as IRC hiding (server, port, nick, password)
import Options.Applicative hiding (action, Success, Failure)
import System.IO
import Toml
import Toml.Syntax
import Toml.Schema.FromValue
import Toml.Schema.Matcher

import Parts.Eval
import Parts.MerriamWebster
import Parts.NLab
import Parts.Random
import Parts.Translate
import Parts.Typst
import Parts.URL
import Parts.WolframAlpha
import Parts.Wikimedia
import Utils

deriving newtype instance Semigroup (Table' l)
deriving newtype instance Monoid (Table' l)

data Options = Options
  { configFile :: FilePath
  , extraConfigFiles :: [FilePath]
  , verbose :: Bool
  }

parseOptions :: Parser Options
parseOptions = do
  configFile <- strOption (long "config" <> short 'c' <> metavar "PATH" <> help "Configuration file to use" <> value "config.toml" <> showDefault)
  extraConfigFiles <- many $ strOption (long "extra-config" <> short 'C' <> metavar "PATH" <> help "Extra configuration files")
  verbose <- switch (long "verbose" <> short 'v' <> help "Log every IRC message to standard output")
  pure Options{..}

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  options <- execParser (info (parseOptions <**> helper) mempty)
  table <- mconcat <$> traverse (either fail pure . parse <=< T.readFile) ([configFile options] <> extraConfigFiles options)
  config <- case runMatcher (fromValue (Table' startPos table)) of
    Success _ x -> pure x
    Failure e -> fail (foldMap prettyMatchMessage e)
  randomCommands <- randomInit
  typstHandler <- typstInit config
  (evalHandler, evalCommands) <- evalInit config
  merriamWebsterCommands <- merriamWebsterInit (merriamWebsterKey config)
  nLabCommands <- nLabInit
  translateCommands <- translateInit
  urlTitleHandler <- urlTitleInit
  wolframAlphaCommands <- wolframAlphaInit (wolframAlphaKey config)
  wikimediaCommands <- wikimediaInit
  let getConnection h p
        | tls config = tlsConnection (WithDefaultConfig h p)
        | otherwise = plainConnection h p
      authenticate pass = send $ Privmsg "NickServ" $ Right $ T.unwords ["IDENTIFY", nick config, pass]
      commands = mconcat [randomCommands, evalCommands, merriamWebsterCommands, nLabCommands, translateCommands, wolframAlphaCommands, wikimediaCommands]
      commandHandler (src@Channel{}, False, msg)
        | Just (cmd:args) <- T.words <$> T.stripPrefix (commandPrefix config) msg
        , Just runCommand <- commands M.!? cmd
        = True <$ runCommand src args
      commandHandler _ = pure False
      messageHandlers = [commandHandler, evalHandler, typstHandler, urlTitleHandler]
      handleMessages = EventHandler matchMessageOrAction \ src (action, msg) -> do
        void . forkWorker $ anyM ($ (src, action, msg)) messageHandlers
      conn = getConnection (encodeUtf8 (server config)) (fromIntegral (port config))
           & username .~ nick config
           & realname .~ realName config
           & onconnect .~ (defaultOnConnect >> for_ (password config) authenticate)
           & logfunc .~ (if verbose options then stdoutLogger else noopLogger)
      cfg  = defaultInstanceConfig (nick config)
           & IRC.channels .~ Utils.channels config
           & handlers <>~ [handleMessages]
  runClient conn cfg config
