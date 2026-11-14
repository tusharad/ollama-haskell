{- |
Module      : Ollama.API.WebSearch
Copyright   : (c) 2024-2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
Stability   : stable
Portability : portable

Ollama Cloud Web Grounding endpoints (@https://ollama.com/api/web_search@ and @https://ollama.com/api/web_fetch@).

Enables real-time internet search and page extraction for local models.
Requires an Ollama account and API key set via @OLLAMA_API_KEY@ or 'Ollama.Client.Config.configApiKey'.

@since 0.4.3.0
-}
module Ollama.API.WebSearch (
  -- * Web Search
  WebSearchRequest (..),
  SearchResult (..),
  WebSearchResponse (..),
  webSearchRequest,
  webSearch,
  webSearchSimple,
  webSearchTool,

  -- * Web Fetch
  WebFetchRequest (..),
  WebFetchResponse (..),
  webFetchRequest,
  webFetch,
  webFetchSimple,
  webFetchTool,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  object,
  withObject,
  (.:),
  (.:?),
  (.=),
 )
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Ollama.Client (OllamaClient (..))
import Ollama.Client.Config (OllamaClientConfig (..))
import Ollama.Client.Internal (request)
import Ollama.Error (OllamaError (..))
import Ollama.Types.Tool (
  Tool (..),
  intParam,
  objectParams,
  stringParam,
  toolDef,
 )

{- | Request payload for Ollama Cloud Web Search.

@since 0.4.3.0
-}
data WebSearchRequest = WebSearchRequest
  { wsrQuery :: !Text
  , wsrMaxResults :: !(Maybe Int)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON WebSearchRequest where
  toJSON WebSearchRequest {..} =
    object $
      catMaybes
        [ ("max_results" .=) <$> wsrMaxResults
        , Just ("query" .= wsrQuery)
        ]

instance FromJSON WebSearchRequest where
  parseJSON = withObject "WebSearchRequest" $ \v ->
    WebSearchRequest
      <$> v .: "query"
      <*> v .:? "max_results"

{- | Create a default 'WebSearchRequest' for a search query.

@since 0.4.3.0
-}
webSearchRequest :: Text -> WebSearchRequest
webSearchRequest query = WebSearchRequest query Nothing

{- | Individual search result returned by Ollama Cloud Web Search.

@since 0.4.3.0
-}
data SearchResult = SearchResult
  { srTitle :: !Text
  , srUrl :: !Text
  , srContent :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \v ->
    SearchResult
      <$> v .: "title"
      <*> v .: "url"
      <*> v .: "content"

instance ToJSON SearchResult where
  toJSON SearchResult {..} =
    object
      [ "title" .= srTitle
      , "url" .= srUrl
      , "content" .= srContent
      ]

{- | Response payload returned by Ollama Cloud Web Search.

@since 0.4.3.0
-}
newtype WebSearchResponse = WebSearchResponse
  { wsResults :: [SearchResult]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON WebSearchResponse where
  parseJSON = withObject "WebSearchResponse" $ \v ->
    WebSearchResponse <$> v .: "results"

instance ToJSON WebSearchResponse where
  toJSON WebSearchResponse {..} =
    object ["results" .= wsResults]

{- | Request payload for fetching the content of a web page URL.

@since 0.4.3.0
-}
newtype WebFetchRequest = WebFetchRequest
  { wfrUrl :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON WebFetchRequest where
  toJSON WebFetchRequest {..} =
    object ["url" .= wfrUrl]

instance FromJSON WebFetchRequest where
  parseJSON = withObject "WebFetchRequest" $ \v ->
    WebFetchRequest <$> v .: "url"

{- | Create a default 'WebFetchRequest' for a web URL.

@since 0.4.3.0
-}
webFetchRequest :: Text -> WebFetchRequest
webFetchRequest = WebFetchRequest

{- | Response payload containing extracted text from a web page.

@since 0.4.3.0
-}
data WebFetchResponse = WebFetchResponse
  { wfrTitle :: !Text
  , wfrContent :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON WebFetchResponse where
  parseJSON = withObject "WebFetchResponse" $ \v ->
    WebFetchResponse
      <$> v .: "title"
      <*> v .: "content"

instance ToJSON WebFetchResponse where
  toJSON WebFetchResponse {..} =
    object
      [ "title" .= wfrTitle
      , "content" .= wfrContent
      ]

-- | Resolve the base URL for Ollama Cloud web services.
webBaseUrl :: OllamaClientConfig -> Text
webBaseUrl cfg
  | "127.0.0.1" `T.isInfixOf` base || "localhost" `T.isInfixOf` base =
      "https://ollama.com"
  | otherwise = base
  where
    base = configBaseUrl cfg

{- | Perform a web search using Ollama Cloud.

Requires an API key configured via @OLLAMA_API_KEY@ or 'Ollama.Client.Config.configApiKey'.

@since 0.4.3.0
-}
webSearch ::
  (MonadIO m) =>
  OllamaClient ->
  WebSearchRequest ->
  m (Either OllamaError WebSearchResponse)
webSearch client req =
  case configApiKey (clientConfig client) of
    Nothing ->
      pure $
        Left
          (InvalidRequest "Ollama API key required for web search. Set OLLAMA_API_KEY or configApiKey.")
    Just _ -> do
      let endpoint = webBaseUrl (clientConfig client) <> "/api/web_search"
      request client "POST" endpoint (Just req)

{- | Convenience helper to perform a web search query string and return results directly.

@since 0.4.3.0
-}
webSearchSimple ::
  (MonadIO m) =>
  OllamaClient ->
  Text ->
  m (Either OllamaError [SearchResult])
webSearchSimple client query = do
  res <- webSearch client (webSearchRequest query)
  pure (wsResults <$> res)

{- | Fetch and extract text content from a web page URL.

Requires an API key configured via @OLLAMA_API_KEY@ or 'Ollama.Client.Config.configApiKey'.

@since 0.4.3.0
-}
webFetch ::
  (MonadIO m) =>
  OllamaClient ->
  WebFetchRequest ->
  m (Either OllamaError WebFetchResponse)
webFetch client req =
  case configApiKey (clientConfig client) of
    Nothing ->
      pure $
        Left
          (InvalidRequest "Ollama API key required for web fetch. Set OLLAMA_API_KEY or configApiKey.")
    Just _ -> do
      let endpoint = webBaseUrl (clientConfig client) <> "/api/web_fetch"
      request client "POST" endpoint (Just req)

{- | Convenience helper to fetch a web page URL and return its text content directly.

@since 0.4.3.0
-}
webFetchSimple ::
  (MonadIO m) =>
  OllamaClient ->
  Text ->
  m (Either OllamaError Text)
webFetchSimple client url = do
  res <- webFetch client (webFetchRequest url)
  pure (wfrContent <$> res)

{- | Callable 'Tool' definition for Ollama models to perform web searches.

@since 0.4.3.0
-}
webSearchTool :: Tool
webSearchTool =
  toolDef
    "web_search"
    "Perform a web search using Ollama Cloud to find real-time information."
    ( objectParams
        [ ("query", stringParam (Just "The search query string to look up."))
        , ("max_results", intParam (Just "Optional maximum number of search results to return (1-10)."))
        ]
        ["query"]
    )

{- | Callable 'Tool' definition for Ollama models to fetch web page content.

@since 0.4.3.0
-}
webFetchTool :: Tool
webFetchTool =
  toolDef
    "web_fetch"
    "Fetch and extract text content from a web page URL."
    ( objectParams
        [ ("url", stringParam (Just "The full HTTP/HTTPS URL of the web page to fetch."))
        ]
        ["url"]
    )
