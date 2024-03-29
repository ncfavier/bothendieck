module Parts.WolframAlpha (wolframAlphaInit) where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Simple
import Network.IRC.Client

import Utils

wolframAlphaInit :: Maybe Text -> IO Commands
wolframAlphaInit (Just key) = do
  let wolframAlphaCommand src args = do
        let query = T.unwords args
            request = parseRequest_ "http://api.wolframalpha.com/v1/result"
                    & setRequestQueryString [ "i"     ?= T.encodeUtf8 query
                                            , "appid" ?= T.encodeUtf8 key
                                            , "units" ?= "metric"
                                            ]
        response <- httpBS request
        replyTo src $ limitOutput $ T.decodeUtf8 $ getResponseBody response
      wolframAlphaSubcommand prefix src args = wolframAlphaCommand src (prefix:args)
  pure $ M.fromList
    [ ("wolframalpha", wolframAlphaCommand)
    , ("wolfram", wolframAlphaCommand)
    , ("wa", wolframAlphaCommand)
    , ("time", wolframAlphaSubcommand "time")
    , ("weather", wolframAlphaSubcommand "weather")
    ]
wolframAlphaInit _ = pure mempty
