module Parts.Eval (evalInit, evalHandler) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable
import GHC.Generics
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
maxOutputLength = 300

evalInit :: IO (Maybe Evaluators)
evalInit = do
  mevaluators <- lookupEnv "EVALUATORS"
  case mevaluators of
    Nothing -> Nothing <$ T.hPutStrLn stderr "warning: EVALUATORS is not set"
    Just evaluatorsPath -> do
      descs <- listDirectory (evaluatorsPath </> "desc")
      Just . mconcat <$> for descs \ desc -> do
        evaluator <- either fail pure =<< eitherDecodeFileStrict' (evaluatorsPath </> "desc" </> desc)
        let path = evaluatorsPath </> "bin" </> desc
        pure . M.fromList $ [(cmd, path) | cmd <- name evaluator : aliases evaluator]

evalHandler :: Evaluators -> EventHandler a
evalHandler evaluators = EventHandler matchMessage \ src msg -> case src of
  Channel _channel _nick
    | (e, T.stripPrefix ">" -> Just (T.strip -> input)) <- T.breakOn ">" msg
    , not (T.null input)
    , Just evaluator <- evaluators M.!? e
    -> void $ do
      (_exitCode, T.pack -> output, _err) <- liftIO $ readCreateProcessWithExitCode (proc evaluator [T.unpack input]) ""
      replyTo src (truncateWithEllipsis maxOutputLength output)
  _ -> pure ()
