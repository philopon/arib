{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

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

class FromBinary a where
  type BinaryRep a
  fromBinary :: BinaryRep a -> a

--------------------------------------------------------------------------------

data ShortEvent = ShortEvent
    { shortEventLanguage    :: {-# UNPACK #-}!S.ByteString
    , shortEventTitle       :: TL.Text
    , shortEventDescription :: TL.Text
    } deriving (Show, Read, Eq, Ord, Typeable)

--------------------------------------------------------------------------------

data Component = Component
    { componentType          :: ComponentType
    , componentTag           :: ComponentTag
    , componentLanguage      :: S.ByteString
    , componentDescription   :: TL.Text
    } deriving (Show, Read, Eq, Ord, Typeable)

data ComponentType 
    = Video { videoResolution  :: VideoResolution
            , videoAspectRatio :: VideoAspectRatio
            }
    | Audio { audioMode :: AudioMode }
    | H264  { h264Resolution  :: VideoResolution
            , h264AspectRatio :: VideoAspectRatio
            }
    | OtherComponentType Word8 Word8
    deriving (Show, Read, Eq, Ord, Typeable)

data VideoResolution
    = Video1080p
    | Video1080i
    | Video720p
    | Video480p
    | Video480i
    | Video240p
    | Video120p
    | Video2160p
    | Video180p
    | OtherResolution Word8
    deriving (Show, Read, Eq, Ord, Typeable)

instance Pretty VideoResolution where
    pretty Video1080p = "1080p"
    pretty Video1080i = "1080i"
    pretty Video720p  = "720p"
    pretty Video480p  = "480p"
    pretty Video480i  = "480i"
    pretty Video240p  = "240p"
    pretty Video120p  = "120p"
    pretty Video2160p = "2160p"
    pretty Video180p  = "180p"
    pretty (OtherResolution w) = "Other[" ++ show w ++ "]"

instance FromBinary VideoResolution where
    type BinaryRep VideoResolution = Word8
    fromBinary w = case shiftR w 4 of
        0x0 -> Video480i
        0x9 -> Video2160p
        0xA -> Video480p
        0xB -> Video1080i
        0xC -> Video720p
        0xD -> Video240p
        0xE -> Video1080p
        0xF -> Video180p
        x   -> OtherResolution x
    {-# INLINE fromBinary #-}

data VideoAspectRatio
    -- | aspect ratio == 4:3
    = Normal
    -- | aspect ratio == 16:9
    | Wide { panVector :: Bool }
    -- | aspect ratio > 16:9
    | Wider
    | OtherAspectRatio Word8
    deriving (Show, Read, Eq, Ord, Typeable)

instance Pretty VideoAspectRatio where
    pretty Normal = "4:3"
    pretty Wide{} = "16:9"
    pretty Wider  = ">16:9"
    pretty (OtherAspectRatio w) = "Other[" ++ show w ++ "]"

instance FromBinary VideoAspectRatio where
    type BinaryRep VideoAspectRatio = Word8
    fromBinary w = case 0xf .&. w of
        1 -> Normal
        2 -> Wide True
        3 -> Wide False
        4 -> Wider
        x -> OtherAspectRatio x
    {-# INLINE fromBinary #-}

data AudioMode
    = AudioMode 
        { audioTop    :: AudioChannel
        , audioMiddle :: AudioChannel
        , audioBottom :: AudioChannel
        , audioLFE    :: {-#UNPACK#-}!Int
        }
    | OtherAudioMode Word8
    deriving (Show, Read, Eq, Ord, Typeable)

data AudioChannel = AudioChannel
    { audioFront :: {-#UNPACK#-}!Int
    , audioSide  :: {-#UNPACK#-}!Int
    , audioBack  :: {-#UNPACK#-}!Int
    } deriving (Show, Read, Eq, Ord, Typeable)

instance FromBinary AudioMode where
    type BinaryRep AudioMode = Word8
    fromBinary 0x01 = AudioMode (AudioChannel 0 0 0) (AudioChannel 1 0 0) (AudioChannel 0 0 0) 0
    fromBinary 0x02 = AudioMode (AudioChannel 0 0 0) (AudioChannel 1 0 0) (AudioChannel 0 0 0) 0 -- TODO: dual mono
    fromBinary 0x03 = AudioMode (AudioChannel 0 0 0) (AudioChannel 2 0 0) (AudioChannel 0 0 0) 0
    fromBinary 0x04 = AudioMode (AudioChannel 0 0 0) (AudioChannel 2 0 1) (AudioChannel 0 0 0) 0
    fromBinary 0x05 = AudioMode (AudioChannel 0 0 0) (AudioChannel 3 0 0) (AudioChannel 0 0 0) 0
    fromBinary 0x06 = AudioMode (AudioChannel 0 0 0) (AudioChannel 2 0 2) (AudioChannel 0 0 0) 0
    fromBinary 0x07 = AudioMode (AudioChannel 0 0 0) (AudioChannel 3 0 1) (AudioChannel 0 0 0) 0
    fromBinary 0x08 = AudioMode (AudioChannel 0 0 0) (AudioChannel 3 0 2) (AudioChannel 0 0 0) 0
    fromBinary 0x09 = AudioMode (AudioChannel 0 0 0) (AudioChannel 3 0 2) (AudioChannel 0 0 0) 1
    fromBinary 0x0A = AudioMode (AudioChannel 0 0 0) (AudioChannel 3 0 3) (AudioChannel 0 0 0) 1
    fromBinary 0x0B = AudioMode (AudioChannel 2 0 0) (AudioChannel 2 0 2) (AudioChannel 0 0 0) 1
    fromBinary 0x0C = AudioMode (AudioChannel 0 0 0) (AudioChannel 5 0 2) (AudioChannel 0 0 0) 1
    fromBinary 0x0D = AudioMode (AudioChannel 0 0 0) (AudioChannel 3 2 2) (AudioChannel 0 0 0) 1
    fromBinary 0x0E = AudioMode (AudioChannel 2 0 0) (AudioChannel 3 0 2) (AudioChannel 0 0 0) 1
    fromBinary 0x0F = AudioMode (AudioChannel 0 2 0) (AudioChannel 3 0 2) (AudioChannel 0 0 0) 1
    fromBinary 0x10 = AudioMode (AudioChannel 2 0 0) (AudioChannel 3 2 3) (AudioChannel 0 0 0) 2
    fromBinary 0x11 = AudioMode (AudioChannel 3 3 3) (AudioChannel 3 2 3) (AudioChannel 0 0 0) 2
    fromBinary w    = OtherAudioMode w
    {-# INLINE fromBinary #-}

instance FromBinary ComponentType where
    type BinaryRep ComponentType = (Word8, Word8)
    fromBinary (0x1, w) = Video (fromBinary w) (fromBinary w)
    fromBinary (0x2, w) = Audio (fromBinary w)
    fromBinary (0x5, w) = H264  (fromBinary w) (fromBinary w)
    fromBinary (a,   b) = OtherComponentType a b
    {-# INLINE fromBinary #-}

-- | B-10 Table.6-5 (pp. 122-126 pdf pp. 134-138)
newtype ComponentTag = ComponentTag Word8 
    deriving (Show, Read, Eq, Ord, Typeable)

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
getDescriptors s = reduceDesc <$> go (Descriptors_ id id id id id id) s
  where
    reduceDesc (Descriptors_ a b c d e f) = Descriptors (a []) (b []) (c []) (d []) (e []) (f [])
    go descs len
        | len <= 0  = return descs
        | otherwise = do
            idesc <- getWord8
            getDescriptor idesc descs >>= \(l, descs') -> go descs' (len - fromIntegral l)
