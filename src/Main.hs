{-# LANGUAGE OverloadedStrings #-}
module Main where

-- | This is holster, a small API for storing and summarizing URLs to a PostgreSQL
-- database. It illustrates how one can plug several requisite pieces (Aeson, Postgres, TagSoup, Snap)
-- into a fun little web app.
--
-- URLS can be POST'd to the root of the application, and displayed with GET. A
-- successful post will contain two pieces: a URL and who did it. For example, using curl on the linux
-- commandline:
--
-- curl -X POST -H "Content-Type: application/json" -d '{"url": "http://news.ycombinator.com/", "person":"qpfiffer"}' http://my-sweet-app.com/
--
-- would respond with:
--
-- {"result":"Successfully posted."}
--
-- There is some limited duplication checking in the form of Holster.DB.urlInDb, so posting the same thing again would result in:
--
-- {"result":"Someone tried to submit a duplicate URL."}
--
-- It's pretty simple to use and look at, and has room for improvements but this is mostly a proof-of-concept.

import           Holster.SnapExtras
import           Holster.Types
import           Holster.DB

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Aeson as A
import qualified Data.Text.Encoding as TE
import           Database.PostgreSQL.Simple
import           Snap.Core
import           Snap.Http.Server hiding (httpServe)
import           Snap.Http.Server.Env


-- | Not covered in the scope of this project is setting up and using PostgreSQL, but all thats required is the following statement:
--
-- CREATE TABLE public.links (id SERIAL PRIMARY KEY NOT NULL,
--     title TEXT,
--     summary TEXT,
--     url TEXT,
--     created_at TIMESTAMPTZ DEFAULT current_timestamp,
--     person TEXT
-- );
--
-- Note: This is a TIMESTAMPTZ because Postgresql simple has trouble
-- or refuses to work with timestamps without timezones. See github issue #74

-- Specify your connection information here. Excercise for the reader: read this information in from a file.
databaseConnection = defaultConnectInfo { connectHost = ""
                                        , connectPort = 5432
                                        , connectDatabase = ""
                                        , connectUser = ""
                                        , connectPassword = ""
                                        }

main :: IO ()
main = do
    -- The FPComplete IDE is cool in that it will sandbox and host your app if you do it
    -- on the correct port. We imported httpServe from Snap.Http.Server.Env, so this will
    -- give you a handy URL when running via the IDE.
    httpServe defaultConfig site
    putStrLn "Bye!"

site :: Snap ()
site =
    -- We only have two simple routes, the submitHandler (which can only be posted to)
    -- and the roothandler, which simply grabs everything from the database and displays it.
    method POST submitHandler <|>
    rootHandler

-- | This function is responsible for grabbing all of the links currently in the database.
getLinkData = connect databaseConnection >>= \conn ->
    query_ conn "SELECT * FROM public.links" >>= \all_links ->
    return (all_links :: [LinkData])

-- | The root handler for the webapp. Simply displays all submitted links.
rootHandler :: Snap ()
rootHandler = do
    -- Grab all of the links from the database, we need to use liftIO here
    -- to make sure we're operating in the Snap monad.
    all_links <- liftIO $ getLinkData
    -- Set the content type to json. This is a web API afterall.
    modifyResponse $ setContentType "application/json"
    -- Take all of the returned links, encode them as JSON using Aeson,
    -- and then write them to the client.
    writeLBS $ A.encode $ (all_links :: [LinkData])

-- | The snap handler for submission. This is responsible for taking POST'd data
-- and then summarizing and storing it in the database.
submitHandler :: Snap ()
submitHandler = do
    -- Snap.Extras.Json has a nifty 'getJSON' function that takes posted data and deserializes
    -- it into a Type. Here we're taking JSON and serializing it to a PostedLinkData type.
    --
    -- Note: The fpcomplete IDE currently doesn't include Snap.Extras.Json, so I reproduced the relevant
    -- functions in Holster.SnapExtras
    posted_link_data <- getJSON :: Snap (Either String PostedLinkData)
    case posted_link_data of
        Right pld -> do
            -- Hey, the deserialization worked. Insert the URL and return the result of that
            -- insertion to the client.
            result <- liftIO $ insertUrl (normalUrl pld) (normalPerson pld) databaseConnection
            modifyResponse $ setContentType "application/json"
            writeLBS $ A.encode $ result
        Left _ -> do
            -- Looks like the deserialization didn't work. Tell the client that.
            modifyResponse $ setContentType "application/json"
            writeLBS $ "Could not decode JSON from POST."
  where
    -- Here we're taking out deserialized data and making sure it's encoded in UTF-8 (we want
    -- to support unicode in our API.)
    normalUrl a = TE.encodeUtf8 $ pldUrl a
    normalPerson a = TE.encodeUtf8 $ pldPerson a