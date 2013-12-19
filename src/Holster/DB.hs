{-# LANGUAGE OverloadedStrings #-}

module Holster.DB where

-- | This is where the functions necessary for database interaction go.

import           Holster.Types
import           Holster.Utils

import           Data.ByteString.Char8 as BSC
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Text.HTML.TagSoup

-- | Takes a URL and a database connection and returns whether or not that URL has been posted before.
urlInDb :: Text -> ConnectInfo -> IO (Either String Bool)
urlInDb test_url database = do
    -- Connect to the database
    conn <- connect database
    -- Query Postgres as to whether or not this URL has been submitted before.
    results <- query conn "SELECT count(*) FROM public.links WHERE url = ? LIMIT 1" (Only test_url)
    -- Doing 'count(*)' will return a single row with a single column. Since we don't actually have a type
    -- to deserialize to, we only care about the first row. It will always be an integer, so we can
    -- get away with using Postgres.Simple's builtin types (Int and [])
    case (results :: [[Int]]) of
        -- Postgres return a count of 0, which means that URL hasn't been submitted before.
        [[0]] -> return $ Right False
        -- We can't have a negative count, so any number other than zero means it's been submitted.
        [[_]] -> return $ Right True
        -- Postgres didn't like something we did, so return an error (Left)
        _ -> return $ Left "Something went wrong checking for duplicate URLS."

-- | This function is responsible for sticking a URL and who posted it into the database.
insertUrl :: BSC.ByteString -> BSC.ByteString -> ConnectInfo -> IO InsertResult
insertUrl posted_url poster db = do
    -- Coerce the ByteString url into UTF-8-compatible text.
    let text_url = TE.decodeUtf8 posted_url
    -- Check to see if the URL is already in the database
    in_db_already <- urlInDb text_url db
    case in_db_already of
        -- It was! Tell the client that.
        (Right True) -> return $ InsertResult "Someone tried to submit a duplicate URL."
        -- Looks like this is a new URL.
        (Right False) -> do
            -- Connect to the database.
            conn <- connect db
            -- Tragically, Network.HTTP doesn't work with HTTPS URLS, so we can't summarize those.
            if (BSC.take 5 posted_url) /= "https"
                then do
                    -- Request the URL and deserialize it into TagSoup's native format
                    tags <- fmap parseTags $ openURL $ BSC.unpack posted_url
                    -- Get the page title of this document
                    let page_title = getTitle tags
                        -- Build the page summary with a length of 300 characters
                        summation = summarize text_url 300 tags
                    -- Insert it into the database
                    _ <- execute conn "INSERT INTO public.links (title, summary, url, person) VALUES (?,?,?,?)" $
                        (page_title, summation, text_url, decoded_poster)
                    -- Everything went alright. Tell the client.
                    return $ InsertResult "Successfully posted."
                else do
                    -- Well it looks like they submitted an HTTPS url, which we can't summarize. We can however still
                    -- submit it to the database in a much simpler format.
                    _ <- execute conn "INSERT INTO public.links (title, summary, url, person) VALUES (?,?,?,?)" $
                        -- Insert the URL with a summary of just "...", and a title of it's URL.
                        ((TE.decodeUtf8 posted_url), "..." :: Text, text_url, decoded_poster)
                    return $ InsertResult "Successfully posted."
        -- Looks like something weird happened when we checked for the URL. Pass that error along to the client.
        (Left msg) -> return $ InsertResult $ T.pack msg
  where
     -- UTF-8 is always nice.
    decoded_poster = TE.decodeUtf8 poster