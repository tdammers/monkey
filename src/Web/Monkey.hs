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
        , lazyRequestBody
        
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
        , ok200
        , created201
        , noContent204
        , badRequest400
        , notFound404
        , methodNotAllowed405
        , conflict409
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

data DeleteResult
  = Deleted
  | DeletedNoItem
  | DeleteNotSupported
  deriving (Show, Enum)

data CreateResult
  = Created Text -- ^ created with ID
  | InvalidItemNotCreated -- ^ not created due to invalid data
  | ConflictingItemNotCreated -- ^ not created due to constraint conflict

data Resource =
  Resource
    { subResources :: View -> IO [(Text, Resource)]
    , getSelf :: IO (Maybe Value)
    , delete :: IO DeleteResult
    , create :: Value -> IO CreateResult
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
  item <- getSelf resource
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

decorateKey :: Path -> Text -> Value
decorateKey path key =
  JSON.object
    [ ("_self" .= (collapsePath $ path <> [key]))
    , ("_key" .= key)
    ]

collapsePath :: Path -> Text
collapsePath = mconcat . map ("/" <>)

getResource :: Resource -> Application
getResource r rq respond = do
  item <- decorateResource (pathInfo rq) r
  respond $
    responseJSON
      ok200
      item

postResource :: Resource -> Application
postResource r rq respond = do
  mValue <- JSON.decode <$> lazyRequestBody rq
  case mValue of
    Nothing ->
      badRequest rq respond
    Just value -> do
      result <- create r value
      case result of
        Created key ->
          respond $
            responseJSON
              created201
              (decorateKey (pathInfo rq) key)
        InvalidItemNotCreated ->
          badRequest rq respond
        ConflictingItemNotCreated ->
          conflictingRequest rq respond

deleteResource :: Resource -> Application
deleteResource r rq respond = do
  result <- delete r
  case result of
    Deleted ->
      respond $
        responseLBS
          noContent204
          []
          ""
    DeletedNoItem ->
      notFound rq respond
    DeleteNotSupported ->
      invalidMethod rq respond

notFound :: Application
notFound rq respond =
  respond $
    responseJSON
      notFound404
      ( [ ("error" .= ("Not Found" :: Text))
        , ("path" .= decodeUtf8 (rawPathInfo rq))
        ] :: HashMap Text Value
      )

invalidMethod :: Application
invalidMethod rq respond = 
  respond $
    responseJSON
      methodNotAllowed405
      ([("error", "Method not allowed")] :: HashMap Text Text)

badRequest :: Application
badRequest rq respond =
  respond $
    responseJSON
      badRequest400
      ([("error", "Bad request")] :: HashMap Text Text)

conflictingRequest :: Application
conflictingRequest rq respond =
  respond $
    responseJSON
      conflict409
      ([("error", "Conflict")] :: HashMap Text Text)

apiToApp :: API -> Application
apiToApp api rq respond
  | pathInfo rq == []
  = case requestMethod rq of
      "GET" -> do
        let body = decorateAPI api
        respond $
          responseJSON
            ok200
            body
apiToApp api rq respond 
  | otherwise
  = case requestMethod rq of
      "GET" -> do
        mResource <- resolveResource (pathInfo rq) api
        maybe
          (notFound rq respond)
          (\r -> getResource r rq respond)
          mResource
      "DELETE" -> do
        mResource <- resolveResource (pathInfo rq) api
        maybe
          (notFound rq respond)
          (\r -> deleteResource r rq respond)
          mResource
      "POST" -> do
        mResource <- resolveResource (pathInfo rq) api
        maybe
          (notFound rq respond)
          (\r -> postResource r rq respond)
          mResource
      _ -> invalidMethod rq respond

responseJSON :: ToJSON a => Status -> a -> Response
responseJSON status x =
  responseLBS
    status
    [("Content-type", "application/json")]
    (JSON.encode x)
