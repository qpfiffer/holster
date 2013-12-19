{-# LANGUAGE OverloadedStrings #-}
module Holster.Utils where

-- | This is the module responsible for miscellenous functions and helpers not big enough for their own files.

import           Data.Char as C
import           Data.Text as T
import qualified Network.HTTP as NH
import           Text.HTML.TagSoup as TS

-- | This function takes a String (a url) and returns a lowercase version of the webpage.
-- We use lowercase because we check against tags like <title> and <body>, and it's not standard
-- that everyone uses the lowercase version.
openURL :: String -> IO String
openURL x = do
    -- Request the webpage using Network.HTTP.simpleHTTP
    response <- NH.simpleHTTP $ NH.getRequest x
    -- Store the webpage in a variable
    response_body <- NH.getResponseBody response
    -- Lowercase it and hand it back
    return $ Prelude.map C.toLower response_body

-- | This function is responsible for extracting the title from TagSoup.
getTitle :: [Tag String] -> T.Text
getTitle tags =
    -- Convert the String title to a Text one
    T.pack title_text
  where
    -- Grab and store the relevant tags (title)
    the_sections = sections (~== ("<title>" :: String)) tags
    -- I've found some web pages don't actually have titles (301 redirects, for instance)
    -- so we need to do a little checking to make sure we can actually get one.
    TagText title_text =
        case Prelude.length the_sections of
            0 -> TagText ("" :: String)
            -- The way the title is actually stored in Tag Soup is a little interesting. It's something like
            -- [[Tag Title][Tag String][Tag /Title]]
            -- which makes sense but results in this little thing.
            _ -> the_sections !! 0 !! 1

-- | This function takes a url, the maximum summary size (I use 300 chars), some tag soup
-- and returns a very dumb page summary. It works by simply grabbing characters in the body that are text
-- tags (p, h1, h2, script, etc.) and putting them together with spaces in between.
--
-- Excercise to the reader: Filter tags out that don't really make sense, maybe only grab p, span and header tags.
summarize :: Text -> Int -> [Tag String] -> T.Text
summarize url summary_size tags =
    case Prelude.length tags of
        -- Somebody passed in some empty tag soup. This usually means a page that couldn't be decoded, like an image.
        -- Return some text anyway.
        0 -> "..."
        -- Take the summary_size and pack it into a Text object.
        _ -> T.pack $ Prelude.take summary_size $
                -- This piece of fun joins together all of our requisite strings with spaces.
                Prelude.foldl (\acc val -> acc ++ " " ++ val) " " $
                    -- Take our soup, filter it for textTags and extract the raw string.
                    Prelude.map fromTagText $ Prelude.filter TS.isTagText relevant_stuff
  where
      -- Doing some extra legwork before summarizing helps to reduce the clutter. Here we're getting
      -- the first body element of the document and only summarizing that. We don't really want a bunch of random
      -- javascript or css (style and script elements both count as 'isTagText' to TagSoup.)
    relevant_stuff = (sections (~== ("<body>" :: String)) tags) !! 0