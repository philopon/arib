{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.Internal.Descriptor where

import Control.Applicative
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap.Strict as IM
import Data.Word
import Data.Binary.Get

import Data.Arib.String

data Descriptor 
    -- | 0x52
    = StreamIdDescriptor {-#UNPACK#-}!Word8
    -- | 0x4D
    | ShortEventDescriptor
        { sedLanguage   :: {-#UNPACK#-}!S.ByteString 
        , sedTitle      :: L.ByteString
        , sedDecription :: L.ByteString
        }
    -- | other descriptors(not implemented.)
    | Raw L.ByteString
    deriving Show

getDescriptor :: Get (Int, Int, Descriptor)
getDescriptor = {-# SCC "getDescriptor" #-} getWord8 >>= \case
    0x52 -> getWord8 >> ((3,0x52,) . StreamIdDescriptor <$> getWord8)
    0x4D -> do
        len  <- getWord8
        lang <- getByteString 3
        pLen <- fromIntegral <$> getWord8
        titl <- decodeUtf8 =<< getLazyByteString pLen
        dLen <- fromIntegral <$> getWord8
        (fromIntegral $ len + 2, 0x4d,) . ShortEventDescriptor lang titl <$> (decodeUtf8 =<< getLazyByteString dLen)
    w    -> getWord8 >>= \len -> (fromIntegral $ len + 2, fromIntegral w,) . Raw <$>
            (getLazyByteString (fromIntegral len))
{-# INLINE getDescriptor #-}

newtype Descriptors = Descriptors (IM.IntMap Descriptor)
                    deriving Show

streamIdDescriptor :: Descriptors -> Maybe Word8
streamIdDescriptor (Descriptors d) = IM.lookup 0x52 d >>= unWrap
  where unWrap (StreamIdDescriptor w) = Just w
        unWrap _ = Nothing
{-# INLINE streamIdDescriptor #-}

getDescriptors :: Int -> Get Descriptors
getDescriptors = {-# SCC "getDescriptors" #-} go IM.empty
  where
    go dict len 
        | len <= 0  = return $ Descriptors dict
        | otherwise = getDescriptor >>= \(l,i,d) -> go (IM.insert i d dict) (len - l)
{-# INLINE getDescriptors #-}
