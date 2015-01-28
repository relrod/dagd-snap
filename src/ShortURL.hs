{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ShortURL where

import           Application
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List (isPrefixOf)
import           Data.Monoid ((<>))
import           Database.Persist.Sql
import           Database.Persist.TH
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Network.URI (isAbsoluteURI)
import           Snap.Snaplet.Persistent
import           System.Random

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Shorturls
  shorturl Text
  longurl Text
  ownerIp Text
  creationDt UTCTime
  enabled Bool Maybe
  customShorturl Bool
  ShorturlKey shorturl
  deriving Eq Ord Read Show
|]

possibleShortUrlChars :: [Char]
possibleShortUrlChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']

isValidShortUrl :: Text -> Either Text Text
isValidShortUrl x = if all (flip elem possibleShortUrlChars) (T.unpack x)
                    then Right x
                    else Left $
                         "Invalid short URL given. Valid characters: "
                         <> T.pack possibleShortUrlChars

isValidLongURL :: Text -> Either Text Text
isValidLongURL x = if liftM2 (&&) (Data.List.isPrefixOf "http") isAbsoluteURI (T.unpack x)
                   then Right x
                   else Left "Invalid long URL given."

isFreeShortUrl :: Text -> AppHandler (Either Text Text)
isFreeShortUrl z = do
  c <- runPersist $ count $ [ShorturlsShorturl ==. z]
  return $ if c == 0
           then Right z
           else Left "That short URL was already taken. :'("

-- Random short URLs --

takePreviousPublicUrl :: Text -> AppHandler (Maybe Text)
takePreviousPublicUrl z = do
  q <- runPersist $ selectFirst [ShorturlsLongurl ==. z, ShorturlsCustomShorturl ==. False] []
  return $ shorturlsShorturl . entityVal <$> q

pickRandomShortUrl :: AppHandler Text
pickRandomShortUrl = r False ""
  where
    r :: Bool -> Text -> AppHandler Text
    r True x = return x
    r False _ = do
      z <- liftIO $ randomShortUrl
      -- TODO: Handle []? (db error?)
      c <- runPersist $ count $ [ShorturlsShorturl ==. T.pack z]
      --[Only c] <- query d "select count(*) from shorturls where shorturl=?" (Only z) :: IO [Only Int]
      r (c == 0) (T.pack z)

    randomShortUrl :: IO String
    randomShortUrl =
      flip replicateM (pick possibleShortUrlChars) =<< randomRIO (2,5)
      where
        pick xs = liftM (xs !!) (randomRIO (0, length xs - 1))


decideShortUrl :: Text -> Maybe Text -> AppHandler (Either Text Text)
decideShortUrl _ (Just shorturl) = do
  case isValidShortUrl shorturl of
    Left e  -> return $ Left e
    Right u -> isFreeShortUrl u
decideShortUrl long Nothing = do
  prev <- takePreviousPublicUrl long
  case prev of
    Nothing -> do
      shorturl <- pickRandomShortUrl
      return $ Right shorturl
    Just s  -> return $ Right s

-- Given a longurl with a shorturl, check if it is valid. If it is, check if it
-- is taken. If it is, error. If not, use it.
--
-- If no shorturl is given, see if the longurl exists in the DB already without
-- being attached to a custom url. If it does, use that record. If not, pick a
-- random url and use it.
