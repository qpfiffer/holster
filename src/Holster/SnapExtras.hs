-- NOTE: This file is essentially just getJSON taken out of Snap.Extras,
-- which isn't currently available in the fpcomplete libs. Read more here:
-- http://hackage.haskell.org/package/snap-extras-0.3/docs/Snap-Extras-JSON.html
module Holster.SnapExtras where

import Data.Aeson as A
import Data.Int
import Snap.Core

getJSON :: (MonadSnap m, A.FromJSON a) => m (Either String a)
getJSON = getBoundedJSON 50000

getBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- A.decode `fmap` readRequestBody n
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case A.fromJSON v of
                A.Error e -> Left e
                A.Success a -> Right a