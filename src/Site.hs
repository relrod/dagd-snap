{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.MysqlSimple
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import           Control.Monad.IO.Class
import           Network.Whois
------------------------------------------------------------------------------
import           Application

------------------------------------------------------------------------------
-- | Decide if we should add a newline or not to the response.
decideStrip :: AppHandler ()
decideStrip = do
  strip <- getParam "strip"
  let shouldStrip = case strip of
        Just "1"    -> True
        Just "true" -> True
        Just ""     -> True
        _           -> False
  unless shouldStrip $ writeBS "\n"

------------------------------------------------------------------------------
-- | Decides how we should handle each result.
-- The algorithm is basically this:
-- If the ?text is passed in the URL and it is either empty or one of:
-- 1/0/true/false, then use its value. If it is passed and it is not one of
-- those values, or if it is not passed at all, then check the Accept header.
-- Default to text/plain, if no Accept header is provided.
prepareContent :: AppHandler ()
prepareContent = do
  text <- getParam "text"
  case text of
    Just ""      -> useText
    Just "1"     -> useText
    Just "true"  -> useText
    Just "0"     -> useHtml
    Just "false" -> useHtml
    _            -> checkHeader
 where
   useText :: AppHandler ()
   useText = do
     modifyResponse $ setContentType "text/plain"
   useHtml :: AppHandler ()
   useHtml = do
     modifyResponse $ setContentType "text/html"
   checkHeader :: AppHandler ()
   checkHeader = do
     r <- getRequest
     case getHeader "Accept" r of
       Just "text/html"             -> useHtml
       Just "application/xhtml+xml" -> useHtml
       _                            -> useText

headersHandler :: AppHandler ()
headersHandler = do
  r <- getRequest
  let headers = listHeaders r
      mapped  = map (\x -> (CI.original (fst x)) `mappend` ": " `mappend` snd x) headers
    in (writeBS $ C8.unlines mapped) >> decideStrip

ipHandler :: AppHandler ()
ipHandler = do
  r <- getRequest
  writeBS $ rqRemoteAddr r
  decideStrip

userAgentHandler :: AppHandler ()
userAgentHandler = do
  r <- getRequest
  writeBS $ fromMaybe "" (getHeader "User-Agent" r)
  decideStrip

whoisHandler :: AppHandler ()
whoisHandler = do
  query <- getParam "query"
  case query of
    Nothing ->
      modifyResponse $ setResponseStatus 404 "Not Found"
    Just q  -> do
      modifyResponse $ setContentType "text/plain"
      w <- liftIO $ whois (C8.unpack q)
      writeText . T.pack . Prelude.unlines $ fmap (fromMaybe "") [fst w, snd w]
      decideStrip

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",           serveDirectory "static")
         , ("/headers",   headersHandler)
         , ("/ip",        ipHandler)
         , ("/ua",        userAgentHandler)
         , ("/w/:query",  whoisHandler)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db mysqlInit
    addRoutes routes
    return $ App h d

