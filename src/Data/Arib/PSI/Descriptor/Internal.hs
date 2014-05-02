{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Arib.PSI.Descriptor.Internal where

import Control.Applicative
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import qualified Data.Text.Lazy as TL

import Data.Typeable
import Data.Word
import Data.Bits
import Data.Binary.Get

import Data.Arib.String

data ShortEvent = ShortEvent
    { shortEventLanguage    :: {-# UNPACK #-}!S.ByteString
    , shortEventTitle       :: TL.Text
    , shortEventDescription :: TL.Text
    } deriving (Show, Read, Eq, Ord, Typeable)

-- | B-10 Table.6-5 (pp. 122-126 pdf pp. 134-138)
newtype ComponentTag
    = ComponentTag Word8 
    deriving (Show, Read, Eq, Ord, Typeable)

type StreamId = ComponentTag

data VideoControl
    = VideoControl 
        { isStillPicture    :: Bool
        , isSequenceEndCode :: Bool
        , videoEncode       :: VideoEncode
        } deriving (Show, Read, Eq, Ord, Typeable)

toVideoControl :: Word8 -> VideoControl
toVideoControl w = VideoControl (testBit w 7) (testBit w 6) $ case shiftR w 2 .&. 0xF of
    0 -> Video1080p
    1 -> Video1080i
    2 -> Video720p
    3 -> Video480p
    4 -> Video480i
    5 -> Video240p
    6 -> Video120p
    7 -> Video2160p
    8 -> Video180p
    x -> Other x

data VideoEncode 
    = Video1080p
    | Video1080i
    | Video720p
    | Video480p
    | Video480i
    | Video240p
    | Video120p
    | Video2160p
    | Video180p
    | Other Word8
    deriving (Show, Read, Eq, Ord, Typeable)

data Descriptors
    = Descriptors
        { 
        -- | 0x4D
          shortEvent   :: [ShortEvent]
        -- | 0x52
        , streamId     :: [StreamId]
        -- | 0xC8
        , videoControl :: [VideoControl]
        , other        :: [(Word8, L.ByteString)]
        } deriving (Show, Read, Eq, Ord, Typeable)

data Descriptors_ 
    = Descriptors_
        { shortEvent_   :: [ShortEvent]            -> [ShortEvent]
        , streamId_     :: [StreamId]              -> [StreamId]
        , videoControl_ :: [VideoControl]          -> [VideoControl]
        , other_        :: [(Word8, L.ByteString)] -> [(Word8, L.ByteString)]
        }

getDescriptor :: Word8 -> Descriptors_ -> Get (Word8, Descriptors_)
getDescriptor 0x52 descs = skip 1 >> getWord8 >>= \w ->
    return (3, descs { streamId_     = streamId_     descs . (ComponentTag   w:) } )

getDescriptor 0xC8 descs = skip 1 >> getWord8 >>= \w -> 
    return (3, descs { videoControl_ = videoControl_ descs . (toVideoControl w:) } )

getDescriptor 0x4D descs = do
    len   <- getWord8
    lang  <- getByteString 3
    pLen  <- fromIntegral <$> getWord8
    title <- either (fail . show) return . decodeText =<< getLazyByteString pLen
    dLen  <- fromIntegral <$> getWord8
    desc  <- either (fail . show) return . decodeText =<< getLazyByteString dLen
    return (len + 2, descs { shortEvent_ = shortEvent_ descs . (:) (ShortEvent lang title desc) } )

getDescriptor w    descs = do
    len <- getWord8
    s   <- getLazyByteString (fromIntegral len)
    return (len + 2, descs { other_ = other_ descs . (:) (w, s) })

getDescriptors :: Int -> Get Descriptors
getDescriptors s = reduceDesc <$> go (Descriptors_ id id id id) s
  where
    reduceDesc (Descriptors_ a b c d) = Descriptors (a []) (b []) (c []) (d [])
    go descs len
        | len <= 0  = return descs
        | otherwise = do
            idesc <- getWord8
            getDescriptor idesc descs >>= \(l, descs') -> go descs' (len - fromIntegral l)
