{-# LANGUAGE OverloadedStrings #-}

module ShortURL
  ( ShortURL (..)
  ) where

import           Control.Applicative
import           Data.Text (Text)
import           Data.Time.LocalTime
import           Database.PostgreSQL.Simple.ToField
import           Snap.Snaplet.PostgresqlSimple

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
