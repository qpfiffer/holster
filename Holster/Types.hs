{-# LANGUAGE OverloadedStrings #-}
module Holster.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson as A
import           Data.Text as T
import           Database.PostgreSQL.Simple.FromRow
import           Data.Time

-- | Type return upon the result of submission.
data InsertResult = InsertResult
    { what_happened :: Text
    }

instance ToJSON InsertResult where
    toJSON (InsertResult result) = object ["result" .= result]

-- | This is the type we deserialize posted data into.
data PostedLinkData = PostedLinkData
    { pldUrl :: Text -- The URL that was posted
    , pldPerson :: Text -- Who posted it
    }

instance FromJSON PostedLinkData where
    parseJSON (Object v) = PostedLinkData <$>
                           v .: "url" <*>
                           v .: "person"
    parseJSON _ = mzero

-- | This is how we store our URL in the database.
data LinkData = LinkData
    { id :: Integer         -- A unique ID for this url
    , title :: Text         -- Title of the web page
    , summary :: Text       -- A short summary
    , url :: Text           -- The URL of the web page
    , created_at :: UTCTime -- The timestamp of when this URL was submitted
    , person :: Text        -- Who submitted the URL.
    } deriving (Show)


instance ToJSON LinkData where
   toJSON (LinkData i t s y c p) =
       object [ "id" .= i
              , "title" .= y
              , "summary" .= s
              , "url" .= t
              , "created_at" .= c
              , "person" .= p
              ]

instance FromRow LinkData where
    fromRow = LinkData <$>
                field <*>
                field <*>
                field <*>
                field <*>
                field <*>
                field