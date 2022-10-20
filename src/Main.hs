module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Encoding
import GHC.Generics
import Network.IRC.Client as IRC hiding (server, port, nick, password)
import Options.Applicative hiding (auto)
import Toml qualified

import Parts.Eval
import Parts.URL

data Options = Options
 { configFile :: FilePath
 , passwordFile :: Maybe FilePath
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
  } deriving (Generic)

parseOptions :: Parser Options
parseOptions = do
  configFile <- strOption (long "config" <> short 'c' <> metavar "PATH" <> help "Configuration file to use" <> value "config.toml" <> showDefault)
  passwordFile <- optional $ strOption (long "password-file" <> short 'p' <> metavar "PATH" <> help "File containing the NickServ password to use")
  verbose <- switch (long "verbose" <> short 'v' <> help "Log every IRC message to standard output")
  pure Options{..}

main :: IO ()
main = do
  options <- execParser (info (parseOptions <**> helper) mempty)
  config <- Toml.decodeFile Toml.genericCodec (configFile options)
  maybePassword <- case passwordFile options of
    Just passFile -> liftIO $ Just <$> T.readFile passFile
    _ -> pure (password config)
  evalState <- evalInit
  urlTitleInit
  let getConnection h p
        | tls config = tlsConnection (WithDefaultConfig h p)
        | otherwise = plainConnection h p
      authenticate pass = send $ Privmsg "NickServ" $ Right $ T.unwords ["IDENTIFY", nick config, pass]
      conn = getConnection (encodeUtf8 (server config)) (fromIntegral (port config))
           & username .~ nick config
           & realname .~ realName config
           & onconnect .~ (defaultOnConnect >> for_ maybePassword authenticate)
           & logfunc .~ (if verbose options then stdoutLogger else noopLogger)
      cfg  = defaultInstanceConfig (nick config)
           & IRC.channels .~ Main.channels config
           & handlers <>~ [ urlTitleHandler ] <> toList (evalHandler <$> evalState)
  runClient conn cfg ()
