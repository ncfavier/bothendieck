module Parts.Eval (evalInit) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.List
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

data Desc = Desc
  { name :: Text
  , aliases :: [Text]
  , mem :: Int
  , available :: [Text]
  } deriving (Generic, FromJSON)

evalInit :: Config -> IO (MessageHandler s Text, Commands)
evalInit config = do
  (evaluators, sort -> names) <- lookupEnv "EVALUATORS" >>= \case
    Nothing -> mempty <$ T.hPutStrLn stderr "warning: EVALUATORS is not set"
    Just evaluatorsPath -> do
      evaluators <- listDirectory (evaluatorsPath </> "desc")
      mconcat <$> for evaluators \ evaluator -> do
        desc <- either fail pure =<< eitherDecodeFileStrict' (evaluatorsPath </> "desc" </> evaluator)
        let path = evaluatorsPath </> "bin" </> evaluator
        pure (M.fromList [(cmd, (desc, path)) | cmd <- name desc : aliases desc], [name desc])
  let evalCommand src [] = do
        replyTo src ("available evaluators: " <> unwordsOrNone names)
      evalCommand src (ev:_)
        | Just (desc, _path) <- evaluators M.!? ev = do
          replyTo src ("name: " <> name desc <>
                       "; aliases: " <> unwordsOrNone (aliases desc) <>
                       "; memory: " <> T.pack (show (mem desc)) <>
                       " MiB; software: " <> unwordsOrNone (available desc))
        | otherwise = replyTo src ("no such evaluator " <> ev)
      handler (src@Channel{}, False, msg)
        | (ev, T.stripPrefix ">" -> Just (T.strip . T.replace "↵" "\n" -> input)) <- T.breakOn ">" msg
        , not (T.null input)
        , Just (_desc, path) <- evaluators M.!? ev
        = True <$ do
          let p = (proc path [T.unpack input]) { env = Just
            [ ("QEVAL_TIME", "15")
            , ("QEVAL_MAX_OUTPUT", "10K")
            ] }
          output <- liftIO $ T.pack <$> readCreateProcess p ""
          if T.compareLength output maxOutputLength <= EQ && length (filter (not . T.null) $ T.lines output) <= maxOutputLines then
            replyTo src output
          else do
            more <- paste config "-" output
            replyTo src $ limitOutput output <> ircBold <> "[" <> more <> "]" <> ircReset
      handler _ = pure False
  pure (handler, M.singleton "eval" evalCommand)
