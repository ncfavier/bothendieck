{-# LANGUAGE OverloadedLists #-}
module Parts.Random (randomInit) where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Network.IRC.Client hiding (nick)

import Utils

randomQuoteCommand :: Vector Text -> Command
randomQuoteCommand qs src@(Channel _channel _nick) args = do
  let target | n:_ <- args, T.length n < 32 = n <> ": "
             | otherwise = ""
  q <- pickRandom qs
  replyTo src (target <> q)
randomQuoteCommand _ _ _ = pure ()

randomInit :: IO Commands
randomInit = do
  pure $ M.fromList
    [ ]
