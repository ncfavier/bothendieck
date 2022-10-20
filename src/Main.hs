module Main where

import Control.Lens
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import GHC.Generics
import Network.IRC.Client as IRC hiding (server, port, nick, password)
import Options.Applicative hiding (auto)
import Toml qualified

import Parts.Eval
import Parts.URL

data Options = Options
 { configFile :: FilePath
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
  verbose <- switch (long "verbose" <> short 'v' <> help "Log every IRC message to standard output")
  pure Options{..}

main :: IO ()
main = do
  options <- execParser (info (parseOptions <**> helper) mempty)
  config <- Toml.decodeFile Toml.genericCodec (configFile options)
  evalState <- evalInit
  urlTitleInit
  let getConnection h p
        | tls config = tlsConnection (WithDefaultConfig h p)
        | otherwise = plainConnection h p
      authenticate
        | Just pass <- password config = send $ Privmsg "NickServ" $ Right $ T.unwords ["IDENTIFY", nick config, pass]
        | otherwise = pure ()
      conn = getConnection (encodeUtf8 (server config)) (fromIntegral (port config))
           & username .~ nick config
           & realname .~ realName config
           & onconnect .~ (defaultOnConnect >> authenticate)
           & logfunc .~ (if verbose options then stdoutLogger else noopLogger)
      cfg  = defaultInstanceConfig (nick config)
           & IRC.channels .~ Main.channels config
           & handlers <>~ [ urlTitleHandler ] <> toList (evalHandler <$> evalState)
  runClient conn cfg ()
