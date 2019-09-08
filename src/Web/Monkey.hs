{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE OverloadedStrings #-}

module Web.Monkey
where

import Network.Wai
        ( Application

        , Request
        , pathInfo
        , rawPathInfo
        , requestMethod
        
        , Response
        , responseLBS
        )
import Data.Text (Text)
import Data.Aeson
        ( FromJSON
        , ToJSON
        , Value
        , (.=)
        )
import qualified Data.Aeson as JSON
import Data.Maybe (listToMaybe)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Network.HTTP.Types
        ( Status
        , status200
        , status404
        , status405
        )
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

type Path = [Text]

data API =
  API
    { apiResources :: [(Text, Resource)]
    }

data View =
  View
    { viewFilter :: ViewFilter
    }
    deriving (Show)

data ViewFilter
  = ViewAll
  | Key Text
  | Prop Text Value
  | And ViewFilter ViewFilter
  | Or ViewFilter ViewFilter
  | Not ViewFilter
  deriving (Show)

defaultView :: View
defaultView =
  View ViewAll

data Resource =
  Resource
    { subResources :: View -> IO [(Text, Resource)]
    , selfItem :: IO (Maybe Value)
    }

resolveResource :: Path -> API -> IO (Maybe Resource)
resolveResource path api =
  go path (apiResources api)
  where
    go [] _ = pure Nothing
    go (x:xs) resources = do
      print (map fst resources)
      print x
      case lookup x resources of
        Nothing ->
          pure Nothing
        Just resource ->
          case xs of
            [] -> pure . Just $ resource
            xs -> go xs =<< subResources resource defaultView

decorateResource :: Path -> Resource -> IO Value
decorateResource path resource = do
  item <- selfItem resource
  let obj = case item of
        Just (JSON.Object o) -> o
        Just x -> [("item", x)]
        Nothing -> []
  children <- subResources resource defaultView
  let childInfo = decorateChildren path (map fst children)
  return . JSON.Object $
    obj <>
    [ ("_self" .= collapsePath path)
    , ("_children" .= childInfo)
    ]

decorateChildren :: Path -> [Text] -> Value
decorateChildren _ [] =
  JSON.Null
decorateChildren path children =
  JSON.object
    [ (name .= (collapsePath $ path <> [name]))
    | name <- children
    ]

decorateAPI :: API -> Value
decorateAPI api =
  JSON.object
    [ ("_self" .= ("/" :: Text))
    , ("_children" .= decorateChildren [] (map fst $ apiResources api))
    ]

collapsePath :: Path -> Text
collapsePath = mconcat . map ("/" <>)

serveResource :: Resource -> Application
serveResource r rq respond = do
  case requestMethod rq of
    "GET" -> do
      item <- decorateResource (pathInfo rq) r
      respond $
        responseJSON
          status200
          item
    _ -> invalidMethod rq respond

notFound :: Application
notFound rq respond =
  respond $
    responseJSON
      status404
      ( [ ("error" .= ("Not Found" :: Text))
        , ("path" .= decodeUtf8 (rawPathInfo rq))
        ] :: HashMap Text Value)

invalidMethod :: Application
invalidMethod rq respond = 
  respond $
    responseJSON
      status405
      ([("error", "Method not allowed")] :: HashMap Text Text)

apiToApp :: API -> Application
apiToApp api rq respond
  | pathInfo rq == []
  = do
      let body = decorateAPI api
      respond $
        responseJSON
          status200
          body
apiToApp api rq respond 
  | otherwise
  = do
      mResource <- resolveResource (pathInfo rq) api
      maybe
        (notFound rq respond)
        (\r -> serveResource r rq respond)
        mResource

responseJSON :: ToJSON a => Status -> a -> Response
responseJSON status x =
  responseLBS
    status
    [("Content-type", "application/json")]
    (JSON.encode x)
