module Parts.Moon (moonInit) where

import Control.Monad.IO.Class
import Data.Astro.Moon
import Data.Astro.Moon.MoonDetails
import Data.Astro.Time.JulianDate
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time
import Network.IRC.Client

import Utils

moonCommand :: Command s
moonCommand src _args = do
  currentTime <- liftIO getCurrentTime
  let (y, mo, d) = toGregorian (utctDay currentTime)
  let TimeOfDay h mi s = timeToTimeOfDay (utctDayTime currentTime)
  let phase = moonPhase j2010MoonDetails $ fromYMDHMS y mo d h mi (realToFrac s)
  replyTo src (T.pack (show phase))

moonInit :: IO (Commands s)
moonInit = pure $ M.fromList [ ("moon", moonCommand) ]
