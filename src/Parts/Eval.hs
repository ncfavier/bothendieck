module Parts.Eval (evalInit, evalHandler) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Traversable
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Simple
import Network.IRC.Client
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

import Utils

type Evaluators = Map Text FilePath

data Desc = Desc
  { name :: Text
  , aliases :: [Text]
  } deriving (Generic, FromJSON)

maxOutputLength :: Int
maxOutputLength = 400

maxOutputLines :: Int
maxOutputLines = 5

evalInit :: IO (Maybe Evaluators)
evalInit = lookupEnv "EVALUATORS" >>= \case
  Nothing -> Nothing <$ T.hPutStrLn stderr "warning: EVALUATORS is not set"
  Just evaluatorsPath -> do
    evaluators <- listDirectory (evaluatorsPath </> "desc")
    Just . mconcat <$> for evaluators \ evaluator -> do
      desc <- either fail pure =<< eitherDecodeFileStrict' (evaluatorsPath </> "desc" </> evaluator)
      let path = evaluatorsPath </> "bin" </> evaluator
      pure $ M.fromList [(cmd, path) | cmd <- name desc : aliases desc]

evalHandler :: Evaluators -> EventHandler a
evalHandler evaluators = EventHandler matchMessage \ src msg -> case src of
  Channel _channel _nick
    | (e, T.stripPrefix ">" -> Just (T.strip -> input)) <- T.breakOn ">" msg
    , not (T.null input)
    , Just evaluator <- evaluators M.!? e
    -> void $ forkWorker do
      (_exitCode, T.pack -> output, _err) <- liftIO $ readCreateProcessWithExitCode (proc evaluator [T.unpack input]) ""
      if T.compareLength output maxOutputLength <= EQ && length (T.lines output) <= maxOutputLines then
        replyTo src output
      else do
        request <- parseRequestThrow "https://0x0.st" >>= formDataBody
          [partFileRequestBody "file" "-" $ RequestBodyBS $ T.encodeUtf8 output]
        response <- httpBS request
        replyTo src $ T.unlines (take maxOutputLines . T.lines $ truncateWithEllipsis maxOutputLength output)
                   <> ircBold <> "[" <> truncateWithEllipsis 100 (T.strip . T.decodeUtf8 $ getResponseBody response) <> "]" <> ircReset
  _ -> pure ()
