module Parts.Translate (translateInit) where

import Control.Monad.IO.Class
import Data.List.Extra
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Network.IRC.Client (replyTo)
import System.Process
import Text.Regex.TDFA

import Utils

maxOutputLength :: Int
maxOutputLength = 400

langs :: Text
langs = "^" <> lang <> ":" <> lang <> "$"
  where lang = "[[:alpha:]-]{0,20}"

parseArgs :: [Text] -> (Text, [Text]) -- ("[source]:[target]", words)
parseArgs (l:ws) | l =~ langs = (l, ws)
parseArgs (unsnoc -> Just (ws, l)) | l =~ langs = (l, ws)
parseArgs ws = (":", ws)

translateCommand :: Command s
translateCommand src args = do
  case parseArgs args of
    (T.splitOn ":" -> [source, target], ws@(_:_)) -> do
      let flags = ["-b", "-no-init", "-no-ansi", "-no-autocorrect"]
                <> (if T.null source then [] else ["-s", T.unpack source])
                <> (if T.null target then [] else ["-t", T.unpack target])
      output <- liftIO $ T.pack . takeWhile (/= '\n') <$> readProcess "trans" (flags <> ["--", T.unpack (T.unwords ws)]) ""
      replyTo src (truncateWithEllipsis maxOutputLength output)
    _ -> replyTo src "usage: translate [[source]:[target]] text | translate text [[source]:[target]]"

translateInit :: IO (Commands s)
translateInit = pure $ M.fromList
  [ ("translate", translateCommand)
  , ("trans", translateCommand)
  , ("tr", translateCommand)
  ]
