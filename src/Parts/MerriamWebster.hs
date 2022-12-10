module Parts.MerriamWebster (merriamWebsterInit) where

import Control.Applicative
import Data.Aeson
import Data.List.Extra
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Network.HTTP.Simple
import Network.IRC.Client
import Text.Read

import Utils

data Entry = Entry
  { headword :: Text
  , function :: Text
  , definitions :: [Text]
  , etymology :: Maybe Text
  } deriving (Show)

data APIResponse = Entries [Entry] | DidYouMean [Text]
  deriving (Show)

instance FromJSON Entry where
  parseJSON v = do
    Object entry <- pure v
    Object hwi <- entry .: "hwi"
    headword <- hwi .: "hw"
    function <- entry .:? "fl" .!= ""
    definitions <- entry .:? "shortdef" .!= []
    et <- entry .:? "et" .!= []
    let etymology = case et of
          Array et0:_ | Data.Aeson.String t <- et0 V.! 1 -> Just t
          _ -> Nothing
    pure Entry{..}

instance FromJSON APIResponse where
  parseJSON v = DidYouMean <$> parseJSON v <|> Entries <$> parseJSON v

renderTokens :: Text -> Text
renderTokens = T.replace "{it}" ircItalic
             . T.replace "{/it}" ircReset
             . T.replace "{b}" ircBold
             . T.replace "{/b}" ircReset
             . T.replace "{bc}" (ircBold <> ": " <> ircReset)
             . T.replace "{ldquo}" "“"
             . T.replace "{rdquo}" "”"

merriamWebsterInit :: Maybe Text -> IO (Commands s)
merriamWebsterInit (Just key) = do
  let wordCommand showEtymology src args = do
        let (entryNumber, T.unwords -> query) = case args of
              "-e":(readMaybe . T.unpack -> Just e):rest -> (e, rest)
              _ -> (1, args)
        let request = parseRequestThrow_ ("https://dictionaryapi.com/api/v3/references/collegiate/json/" <> T.unpack query)
                    & setRequestQueryString ["key" ?= T.encodeUtf8 key]
        response <- httpJSON request
        case getResponseBody response of
          DidYouMean [] -> replyTo src "no results"
          DidYouMean suggestions -> replyTo src ("did you mean: " <> T.intercalate " | " suggestions)
          Entries entries -> do
            let n = length entries
            case entries !? pred entryNumber of
              Just entry -> replyTo src $ limitOutput $
                ircBold <> T.replace "*" "" (headword entry) <> ircReset
                <> (if n > 1 then " (" <> T.pack (show entryNumber) <> "/" <> T.pack (show n) <> ")" else "")
                <> (if T.null (function entry) then "" else " " <> ircUnderline <> function entry <> ircReset)
                <> " : " <> (if showEtymology then fromMaybe "no etymology" (renderTokens <$> etymology entry) else T.intercalate " | " (definitions entry))
              _ -> replyTo src "invalid entry"
      defineCommand = wordCommand False
      etymologyCommand = wordCommand True
  pure $ M.fromList
    [ ("define", defineCommand)
    , ("def", defineCommand)
    , ("d", defineCommand)
    , ("etymology", etymologyCommand)
    , ("etym", etymologyCommand)
    ]
merriamWebsterInit _ = pure mempty
