module Parts.Random where

import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Network.IRC.Client
import Unicode.Char.General.Scripts

import Utils

scripts :: Vector Script
scripts = V.fromList [minBound..maxBound]

randomCommand :: Command
randomCommand src [] = replyTo src "random what?"
randomCommand src args = do
  scr <- pickRandom scripts
  char <- pickRandom (V.fromList (scriptDefinition scr))
  liftIO $ print (scr, char)
  replyTo src (T.singleton char)

randomInit :: IO Commands
randomInit = pure $ M.singleton "random" randomCommand
