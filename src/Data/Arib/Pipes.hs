{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Arib.Pipes where

import System.IO (Handle)
import qualified Pipes as P
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder.Extra ( defaultChunkSize )
import Data.Word

fromStrict :: Monad m => S.ByteString -> P.Producer' Word8 m ()
fromStrict = S.foldr' (\w a -> P.yield w >> a) (return ())
{-# INLINE fromStrict #-}

fromLazy :: Monad m => L.ByteString -> P.Producer' Word8 m ()
fromLazy = L.foldrChunks (\s a -> fromStrict s >> a) (return ())
{-# INLINE fromLazy #-}

fromHandle :: P.MonadIO m => Handle -> P.Producer' Word8 m ()
fromHandle h = go
  where
    go = do
        bs <- P.liftIO (S.hGetSome h defaultChunkSize)
        if S.null bs
            then return ()
            else fromStrict bs >> go

{-# INLINE fromHandle #-}

