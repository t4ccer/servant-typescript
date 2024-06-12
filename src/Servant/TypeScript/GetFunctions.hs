{-# LANGUAGE ImportQualifiedPost #-}

module Servant.TypeScript.GetFunctions (
  getFunctions
  ) where

import Control.Lens ((^.))
import Data.ByteString.Char8 qualified as Char8
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Servant.Foreign.Internal


-- | Default implementation of @getFunctions@.
getFunctions :: (Req Text -> Text) -> [Req Text] -> Text
getFunctions getFunctionName reqs =
  "import queryString from \"query-string\";\n\n"
  <> T.intercalate "\n" (fmap (reqToFunction getFunctionName) reqs)

reqToFunction :: (Req Text -> Text) -> Req Text -> Text
reqToFunction getFunctionName req =
  T.unlines
    [ "export function " <> getFunctionName req <> getGenericBrackets req <> "(url: string, " <> getFunctionArgs req <> ": Promise<" <> getReturnType req <> "> {"
    , "  const options: RequestInit = {"
    , "    method: \"" <> ( T.pack $ Char8.unpack (req ^. reqMethod)) <> "\","
    , "    headers: {\"Content-Type\": \"application/json;charset=utf-8\"}"
    , "  };"
    , "  " <> case (req ^. reqBody) of Nothing -> ("" :: Text); Just _ -> "\n  options.body = JSON.stringify(body);\n"
    , "  const params = " <> T.intercalate ", " (getQueryParamNames req)
    , "  return fetch(url + `" <> getPath req <> "` + \"?\" + queryString.stringify(params), options).then((response) => {"
    , "    return new Promise((resolve, reject) => {"
    , "      if (response.status !== 200) {"
    , "        return response.text().then((text) =1> reject({text, status: response.status}));"
    , "      } else {"
    , "        " <> if hasReturn req
                    then ("return response.json().then((json) => resolve(json));" :: Text)
                    else "resolve();"
    , "      }"
    , "    });"
    , "  });"
    , "}"
    ]

hasReturn :: Req Text -> Bool
hasReturn req = case req ^. reqReturnType of
  Nothing -> False
  Just "void" -> False
  Just _ -> True

getQueryParamNames :: Req Text -> [Text]
getQueryParamNames req = [x ^. (queryArgName . argName . _PathSegment)
                         | x <- req ^. (reqUrl . queryStr)]

getFunctionArgs :: Req Text -> Text
getFunctionArgs req = T.intercalate ", " $ catMaybes $
  maybeBodyArg
  : fmap formatCaptureArg (req ^. (reqUrl . path))
  <> fmap (Just . formatQueryArg) (req ^. (reqUrl . queryStr))
  where
    maybeBodyArg = case req ^. reqBody of
      Nothing -> Nothing
      Just x -> Just ("body: " <> x)

formatCaptureArg :: Segment Text -> Maybe Text
formatCaptureArg (Segment (Static {})) = Nothing
formatCaptureArg (Segment (Cap arg)) =
  Just (arg ^. (argName . _PathSegment) <> ": " <> arg ^. argType)

formatQueryArg :: QueryArg Text -> Text
formatQueryArg arg = case arg ^. queryArgType of
  Normal -> name <> "?: " <> typ
  Flag -> name <> "?: boolean"
  List -> name <> "?: [" <> typ <> "]"
  where
    qaName = arg ^. queryArgName
    name = qaName ^. (argName . _PathSegment)
    typ = qaName ^. argType

getReturnType :: Req Text -> Text
getReturnType req = fromMaybe "void" (req ^. reqReturnType)

getGenericBrackets :: Req Text -> Text
getGenericBrackets _req = ""

getPath :: Req Text -> Text
getPath req = "/" <> T.intercalate "/" (fmap formatPathSegment (req ^. (reqUrl . path)))
  where
    formatPathSegment :: Segment Text -> Text
    formatPathSegment (Segment (Static (PathSegment t))) = t
    formatPathSegment (Segment (Cap ((^. argName) -> (PathSegment t)))) = "${" <> t <> "}"
