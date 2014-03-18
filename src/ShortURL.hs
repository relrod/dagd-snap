{-# LANGUAGE OverloadedStrings #-}

module ShortURL
  ( ShortURL (..)
  , decideShortUrl
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.List (isPrefixOf)
import           Data.Monoid (mappend)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Network.URI (isAbsoluteURI)
import           Snap.Snaplet.PostgresqlSimple hiding (query)
import           System.Random

data ShortURL = ShortURL {
  id         :: Maybe Integer
, shorturl   :: String
, longurl    :: Text
, ownerIp    :: String
, creationDt :: Maybe LocalTime
, enabled    :: Maybe Bool -- TODO:  schema change to not null
, custom     :: Maybe Bool -- TODO:  schema change to not null
} deriving (Show, Eq)

instance FromRow ShortURL where
  fromRow =
    ShortURL <$>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field <*>
    field

instance ToRow ShortURL where
  toRow u = [ toField (shorturl u)
            , toField (longurl u)
            , toField (ownerIp u)
            , toField (enabled u)
            , toField (custom u)
            ]

possibleShortUrlChars :: [Char]
possibleShortUrlChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']

isValidShortUrl :: ByteString -> Either ByteString ByteString
isValidShortUrl x = if all (flip elem possibleShortUrlChars) (C8.unpack x)
                    then Right x
                    else Left $
                         "Invalid short URL given. Valid characters: "
                         `mappend` C8.pack possibleShortUrlChars

isValidLongURL :: ByteString -> Either ByteString ByteString
isValidLongURL x = if liftM2 (&&) (Data.List.isPrefixOf "http") isAbsoluteURI (C8.unpack x)
                   then Right x
                   else Left "Invalid long URL given."

isFreeShortUrl :: Connection -> ByteString -> IO (Either ByteString ByteString)
isFreeShortUrl d z = do
  c <- query d "select count(*) from shorturls where shorturl=?" (Only z) :: IO [Only Int]
  case c of
    [Only c] -> if c == 0
           then return (Right z)
           else return (Left "That short URL was already taken. :'(")
    _   -> return $ Left "Unable to check the uniqueness of this shorturl. :'("

-- Random short URLs --

takePreviousPublicUrl :: Connection -> ByteString -> IO (Maybe ByteString)
takePreviousPublicUrl d z = do
  q <- query d "select shorturl from shorturls where longurl=? and custom_shorturl=false" (Only (C8.unpack z)) :: IO [Only String]
  return $ case q of
    [Only shorturl] -> Just (C8.pack shorturl)
    _               -> Nothing

pickRandomShortUrl :: Connection -> IO ByteString
pickRandomShortUrl d = r False ""
  where
    r :: Bool -> String -> IO ByteString
    r True x = return (C8.pack x)
    r False _ = do
      z <- randomShortUrl
      -- TODO: Handle []? (db error?)
      [Only c] <- query d "select count(*) from shorturls where shorturl=?" (Only z) :: IO [Only Int]
      r (c == 0) z

    randomShortUrl :: IO String
    randomShortUrl =
      flip replicateM (pick possibleShortUrlChars) =<< randomRIO (2,5)
      where
        pick xs = liftM (xs !!) (randomRIO (0, length xs - 1))

decideShortUrl :: Connection -> ByteString -> Maybe ByteString -> IO (Either ByteString ByteString)
decideShortUrl d _ (Just shorturl) = do
  case isValidShortUrl shorturl of
    Left e  -> return $ Left e
    Right u -> isFreeShortUrl d u
decideShortUrl d long Nothing = do
  prev <- takePreviousPublicUrl d long
  case prev of
    Nothing -> do
      shorturl <- pickRandomShortUrl d
      return $ Right shorturl
    Just s  -> return $ Right s

-- Given a longurl with a shorturl, check if it is valid. If it is, check if it
-- is taken. If it is, error. If not, use it.
--
-- If no shorturl is given, see if the longurl exists in the DB already without
-- being attached to a custom url. If it does, use that record. If not, pick a
-- random url and use it.
