{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Arib.PSI.Descriptor.Component where

import qualified Data.ByteString as S
import qualified Data.Text.Lazy as TL
import Data.Typeable
import Data.Bits
import Data.Word
import Data.Arib.PSI.Internal.Common

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

-- | B-10 Table.6-5 (pp. 122-126 pdf pp. 134-138)
newtype ComponentTag = ComponentTag Word8 
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

data AudioChannel = AudioChannel
    { audioFront :: {-#UNPACK#-}!Int
    , audioSide  :: {-#UNPACK#-}!Int
    , audioBack  :: {-#UNPACK#-}!Int
    } deriving (Show, Read, Eq, Ord, Typeable)

instance FromBinary ComponentType where
    type BinaryRep ComponentType = (Word8, Word8)
    fromBinary (0x1, w) = Video (fromBinary w) (fromBinary w)
    fromBinary (0x2, w) = Audio (fromBinary w)
    fromBinary (0x5, w) = H264  (fromBinary w) (fromBinary w)
    fromBinary (a,   b) = OtherComponentType a b
    {-# INLINE fromBinary #-}



