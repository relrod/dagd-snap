{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import           Data.Char (chr)
import           Data.List (dropWhileEnd, intercalate, nub)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Graphics.ImageMagick.MagickWand
import qualified Network.HTTP.Conduit as NHC
import           Network.URI (isAbsoluteURI, isIPv4address, isIPv6address)
import qualified Network.Socket as S
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import qualified Snap.Snaplet.PostgresqlSimple as PG
import           Snap.Util.FileServe
import qualified System.Random as Rand
import qualified Text.Regex.PCRE.Light as PCRE
import           Heist
import qualified Heist.Interpreted as I
import           Control.Monad.IO.Class
import           Network.Whois
------------------------------------------------------------------------------
import           Application
import qualified ShortURL
import           ShortURL (ShortURL ())

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

siteHeadersHandler :: AppHandler ()
siteHeadersHandler = do
  r <- getRequest
  let site = rqPathInfo r
  case site of
    "" -> do
      let headers = listHeaders r
          mapped  = map (\x -> (CI.original (fst x)) `mappend` ": " `mappend` snd x) headers
        in (writeBS $ C8.unlines mapped)
    s  -> do
      rsp <- liftIO $ do
        u <- NHC.parseUrl (C8.unpack $ appendHttp s)
        NHC.withManager $ NHC.httpLbs u
      writeText . T.pack . unlines $
        fmap (\(a, b) ->
          C8.unpack $ CI.original a `mappend` ": " `mappend` b) (NHC.responseHeaders rsp)
  decideStrip
 where
   appendHttp x =
     if "http://" `C8.isPrefixOf` x || "https://" `C8.isPrefixOf` x
     then x
     else C8.append "http://" x

hostHandler :: AppHandler ()
hostHandler = do
  host <- getParam "host"
  case host of
    Nothing ->
      modifyResponse $ setResponseStatus 404 "Not Found"
    Just host' -> do
      if isIpAddress (C8.unpack host')
        then do
          h <- liftIO $
               fmap (S.addrAddress . head) $
               S.getAddrInfo Nothing (C8.unpack <$> host) Nothing
          name <- liftIO $
                  fmap fst $ S.getNameInfo [] True False h
          writeText $
            T.pack (fromMaybe "Unable to determine reverse DNS." name)
        else do
          ips <- liftIO $
                 try (fmap (fmap (init . dropWhileEnd (/= ':') . show . S.addrAddress)) $
                 S.getAddrInfo Nothing (C8.unpack <$> host) Nothing)
                 :: AppHandler (Either SomeException [String])
          case ips of
            Left e -> writeBS "Unable to get any IPs for the given hostname."
            Right res -> writeText $ T.pack (intercalate ", " (nub res))
      decideStrip
 where
   isIpAddress :: String -> Bool
   isIpAddress = liftM2 (||) isIPv4address isIPv6address


imageHandler :: AppHandler ()
imageHandler = do
  bgColor <- getParam "bgcolor"
  text    <- getParam "text"
  r       <- getRequest
  let path = rqPathInfo r
  let regex = PCRE.compile "^(\\d+)[x*](\\d+)(?:\\.|/|)(\\w+)?/?$" [PCRE.caseless]
  case PCRE.match regex path [] of
    Just (_:width:height:t) -> do
      let m = fromMaybe (width `mappend` "x" `mappend` height) text
          extension = if null t then "png" else head t
      img <- liftIO $ withMagickWandGenesis $ do
        (_,w)  <- magickWand
        (_,dw) <- drawingWand
        c      <- pixelWand
        c `setColor` C8.append (C8.pack "#") (fromMaybe "2a4a77" bgColor)
        newImage w (read $ C8.unpack width) (read $ C8.unpack height) c
        c `setColor` "white"
        dw `setFillColor` c
        dw `setTextAntialias` True
        dw `setStrokeOpacity` 0
        drawAnnotation dw 20 30 (T.pack $ C8.unpack m)
        drawImage w dw
        w `setImageFormat` T.pack (C8.unpack extension)
        getImageBlob w
      mime <- liftIO $ withMagickWandGenesis $ toMime (T.pack $ C8.unpack extension)
      modifyResponse $ setContentType (C8.pack $ T.unpack mime)
      writeBS img
    _       -> modifyResponse $ setResponseStatus 404 "Not Found"

ipHandler :: AppHandler ()
ipHandler = do
  r <- getRequest
  writeBS $ rqRemoteAddr r
  decideStrip

-- | Generate random-ish passwords.
--
-- If "xkcd" is given, we will render xkcd-style passwords using random words
-- from /usr/share/dict/words. Otherwise, we will render a random string.
--
-- For xkcd-style, length controls the number of words. For random-style, length
-- controls the length of the output string.
passwordHandler :: AppHandler ()
passwordHandler = do
  xkcd <- getParam "xkcd"
  length <- getParam "length"

  let useXkcd =
        case xkcd of
          Just ""     -> True
          Just "1"    -> True
          Just "true" -> True
          _           -> False
      pwLength =
        case length of
          Nothing -> if useXkcd then 4 else 25
          Just n  -> read (C8.unpack n) :: Int
  if useXkcd
    then do
      words <- liftIO $ C8.lines <$> C8.readFile "/usr/share/dict/words"
      password <- liftIO $ replicateM pwLength (pick words)
      writeBS $ C8.unwords password
    else do
      s <- liftIO $ replicateM pwLength (pick possibleChars)
      writeBS $ C8.pack s
  decideStrip
  where
    possibleChars = map chr [33..126]
    pick xs = liftM (xs !!) (Rand.randomRIO (0, length xs - 1))

statusHandler :: AppHandler ()
statusHandler = do
  code    <- getParam "code"
  message <- getParam "message"

  case code of
    Nothing ->
      modifyResponse $ setResponseStatus 404 "Not Found"
    Just c  ->
      modifyResponse $
        setResponseStatus (read $ C8.unpack c) (fromMaybe "da.gd header test" message)

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

shortUrlRedirectHandler :: AppHandler ()
shortUrlRedirectHandler = do
  shorturl <- getParam "shorturl"
  case shorturl of
    Nothing ->
      modifyResponse $ setResponseStatus 404 "Not Found"
    Just s  -> do
      result <- PG.query "select * from shorturls where shorturl=?" (PG.Only s)
      case result :: [ShortURL] of
        []    -> modifyResponse $ setResponseStatus 404 "Not Found"
        [url] -> do
          r <- getRequest
          let longUrl = C8.pack $ T.unpack (ShortURL.longurl url)
              extra   = C8.drop (C8.length (rqContextPath r) - 1) (rqURI r)
            in redirect (longUrl `mappend` extra)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",                       serveDirectory "static")
         , ("/headers",               siteHeadersHandler)
         , ("/host/:host",            hostHandler)
         , ("/image",                 imageHandler)
         , ("/ip",                    ipHandler)
         , ("/password",              passwordHandler)
         , ("/status/:code",          statusHandler)
         , ("/status/:code/:message", statusHandler)
         , ("/ua",                    userAgentHandler)
         , ("/w/:query",              whoisHandler)
         , ("/:shorturl",             shortUrlRedirectHandler)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db PG.pgsInit
    addRoutes routes
    return $ App h d

