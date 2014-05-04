{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Arib.PSI.Descriptor.Internal where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import qualified Data.Text.Lazy as TL

import Data.Typeable
import Data.Word
import Data.Bits
import Data.Binary.Get

import Data.Arib.PSI.Internal.Common
import Data.Arib.String
import Data.Arib.PSI.Descriptor.Component
import Data.Arib.PSI.Descriptor.Content

--------------------------------------------------------------------------------

data ShortEvent = ShortEvent
    { shortEventLanguage    :: {-# UNPACK #-}!S.ByteString
    , shortEventTitle       :: TL.Text
    , shortEventDescription :: TL.Text
    } deriving (Show, Read, Eq, Ord, Typeable)

--------------------------------------------------------------------------------

type StreamId = ComponentTag

--------------------------------------------------------------------------------

data VideoDecodeControl
    = VideoDecodeControl 
        { isStillPicture    :: Bool
        , isSequenceEndCode :: Bool
        , videoEncodeFormat :: VideoResolution
        } deriving (Show, Read, Eq, Ord, Typeable)

instance FromBinary VideoDecodeControl where
    type BinaryRep VideoDecodeControl = Word8
    fromBinary w = VideoDecodeControl (testBit w 7) (testBit w 6) $ case shiftR w 2 .&. 0xF of
        0 -> Video1080p
        1 -> Video1080i
        2 -> Video720p
        3 -> Video480p
        4 -> Video480i
        5 -> Video240p
        6 -> Video120p
        7 -> Video2160p
        8 -> Video180p
        x -> OtherResolution x
    {-# INLINE fromBinary #-}

--------------------------------------------------------------------------------

data EventGroup = EventGroup
    { eventGroupType   :: GroupType
    , serviceEventMap  :: [(Word16, Word16)]
    , eventPrivateData :: L.ByteString -- TODO: relay to/move from
    } deriving (Show, Read, Eq, Ord, Typeable)

data GroupType 
    = EventShare
    | EventRelay
    | EventMove
    | EventRelayToOtherNetwork
    | EventMoveFromOtherNetwork
    | Undefined Word8
    deriving (Show, Read, Eq, Ord, Typeable)

instance FromBinary GroupType where
    type BinaryRep GroupType = Word8
    fromBinary w = case w of
        0x1 -> EventShare
        0x2 -> EventRelay
        0x3 -> EventMove
        0x4 -> EventRelayToOtherNetwork
        0x5 -> EventMoveFromOtherNetwork
        x   -> Undefined x
    {-# INLINE fromBinary #-}

--------------------------------------------------------------------------------

data Descriptors
    = Descriptors
        { 
        -- | 0x4D
          shortEvent         :: [ShortEvent]
        -- | 0x50
        , component          :: [Component]
        -- | 0x52
        , streamId           :: [StreamId]
        -- | 0x54
        , content            :: [Content]
        -- | 0xC8
        , videoDecodeControl :: [VideoDecodeControl]
        -- | 0xD6
        , eventGroup         :: [EventGroup]
        , other              :: [(Word8, L.ByteString)]
        } deriving (Show, Read, Eq, Ord, Typeable)

data Descriptors_ 
    = Descriptors_
        { shortEvent_         :: [ShortEvent]            -> [ShortEvent]
        , component_          :: [Component]             -> [Component]
        , streamId_           :: [StreamId]              -> [StreamId]
        , content_            :: [Content]               -> [Content]
        , videoDecodeControl_ :: [VideoDecodeControl]    -> [VideoDecodeControl]
        , eventGroup_         :: [EventGroup]            -> [EventGroup]
        , other_              :: [(Word8, L.ByteString)] -> [(Word8, L.ByteString)]
        }

getDescriptor :: Word8 -> Descriptors_ -> Get (Word8, Descriptors_)
getDescriptor 0x4D descs = do
    len   <- getWord8
    lang  <- getByteString 3
    pLen  <- fromIntegral <$> getWord8
    title <- either (fail . show) return . decodeText =<< getLazyByteString pLen
    dLen  <- fromIntegral <$> getWord8
    desc  <- either (fail . show) return . decodeText =<< getLazyByteString dLen
    return (len + 2, descs { shortEvent_ = shortEvent_ descs . (:) (ShortEvent lang title desc) } )

getDescriptor 0x50 descs = do
    len  <- getWord8
    sc   <- (0xF .&.) <$> getWord8
    cty  <- getWord8
    ctg  <- ComponentTag <$> getWord8
    lang <- getByteString 3
    desc <- either (fail . show) return . decodeText =<< getLazyByteString (fromIntegral len - 6)
    return $ (len + 2, descs { component_ = component_ descs . (:) (Component (curry fromBinary sc cty) ctg lang desc) } )

getDescriptor 0x52 descs = skip 1 >> getWord8 >>= \w ->
    return (3, descs { streamId_     = streamId_     descs . (ComponentTag   w:) } )

getDescriptor 0x54 descs = do
    genre <- skip 1 >> fromBinary <$> getWord8
    un    <- getWord8
    return $ (4, descs { content_ = content_ descs . (Content genre un:) } )

getDescriptor 0xC8 descs = skip 1 >> getWord8 >>= \w -> 
    return (3, descs { videoDecodeControl_ = videoDecodeControl_ descs . (fromBinary w:) } )

getDescriptor 0xD6 descs = do
    len  <- getWord8
    gtec <- getWord8
    sem  <- replicateM (fromIntegral $ gtec .&. 0xF) ((,) <$> getWord16be <*> getWord16be)
    pd   <- getLazyByteString . fromIntegral $ len - 1 - (gtec .&. 0xF) * 4
    return $ (len + 2, descs { eventGroup_ = eventGroup_ descs . (:) (EventGroup (fromBinary $ shiftR gtec 4) sem pd) })

getDescriptor w    descs = do
    len <- getWord8
    s   <- getLazyByteString (fromIntegral len)
    return (len + 2, descs { other_ = other_ descs . (:) (w, s) })

getDescriptors :: Int -> Get Descriptors
getDescriptors s = reduceDesc <$> go (Descriptors_ id id id id id id id) s
  where
    reduceDesc (Descriptors_ a b c d e f g) = Descriptors (a []) (b []) (c []) (d []) (e []) (f []) (g [])
    go descs len
        | len <= 0  = return descs
        | otherwise = do
            idesc <- getWord8
            getDescriptor idesc descs >>= \(l, descs') -> go descs' (len - fromIntegral l)
