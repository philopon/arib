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

data AudioComponent = AudioComponent
    { audioComponentType        :: AudioMode
    , audioComponentTag         :: ComponentTag
    , audioStreamType           :: StreamType 
    , simulcastGroupTag         :: Maybe Word8
    , esMultilingualFlag        :: Bool
    , mainComponentFlag         :: Bool
    , qualityIndicator          :: Word8
    , samplingRate              :: Int
    , audioComponentLangauge    :: S.ByteString
    , audioComponentLangauge2   :: Maybe S.ByteString
    , audioComponentDescription :: TL.Text
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

-- | B-10 table E-4 (pp.216-217, pdf pp. 228-229)
data StreamType 
    -- | 0x00, 0x21-0x7E, 0x80-0x90, 0x92-0xC3
    = UndefinedStreamType Word8
    -- | 0x01 ISO/IEC 11172-2 Video
    | Mpeg1Video
    -- | 0x02 ITU-T recomended H.262|ISO/IEC 13818-2 Video or 
    -- ISO/IEC 11172-2 restricted parameter video stream
    | Mpeg2Video
    -- | 0x03 ISO/IEC 11172-2 Audio
    | Mpeg1Audio
    -- | 0x04 ISO/IEC 13818-3 Audio
    | Mpeg2Audio
    -- | 0x05 ITU-T recomended H.222.0|ISO/IEC 13818-1 private section
    | Mpeg2PrivateSection
    -- | 0x06 ITU-T recomended H.222.0 contained private data|ISO/IEC 13818-1 PES Packet
    | Mpeg2PESPacket
    -- | 0x07 ISO/IEC13522 MHEG
    | MHEG
    -- | 0x08 ITU-T recomended H.222.0|ISO/IEC 13818-1 appendex.A DSM-CC
    | DSMCC
    -- | 0x09 ITU-T recomended H.222.1
    | H222_1
    -- | 0x0A ISO/IEC 13818-6 type A
    | DSMCCTypeA
    -- | 0x0B ISO/IEC 13818-6 type B
    | DSMCCTypeB
    -- | 0x0C ISO/IEC 13818-6 type C
    | DSMCCTypeC
    -- | 0x0D ISO/IEC 13818-6 type D
    | DSMCCTypeD
    -- | 0x0E ISO/IEC 13818-6 other data type
    | DSMCCOtherType
    -- | 0x0F ISO/IEC 13818-7 Audio (ADTS transport structure)
    | Mpeg2ADTSAudio
    -- | 0x10 ISO/IEC 14496-2 Video
    | Mpeg4Video
    -- | 0x11 ISO/IEC 14496-2 Audio (LATM transport structure specified in ISO/IEC 14496-3/AMD 1)
    | LATMAudio
    -- | 0x12 ISO/IEC 14496-1 CL packetized stream or flex max stream sent by PES packet
    | StreamOnPESPacket
    -- | 0x13 ISO/IEC 14496-1 CL packetized stream or flex max stream sent by ISO/IEC 14496 section
    | StreamOnMpeg4Section
    -- | 0x14 ISO/IEC 13818-6 sync download protocol
    | Mpeg2SyncDownload
    -- | 0x15 Metadata sent by PES Packet
    | MetadataOnPESPacket
    -- | 0x16 Metadata send by metadata section
    | MetadataOnSection
    -- | 0x17 Metadata send by ISO/IEC 13818-6 data carousel
    | MetadataOnMpeg2DataCarousel
    -- | 0x18 Metadata send by ISO/IEC 13818-6 object carousel
    | MetadataOnMpeg2ObjectCarousel
    -- | 0x19 Metadata send by ISO/IEC 13818-6 object carousel
    | MetadataOnMpeg2SyncDownload
    -- | 0x1A,0x7F IPMP stream(Mpeg-2 IPMP specified in ISO/IEC 13818-11)
    | IPMPStream Word8
    -- | 0x1B AVC video stream specified in ITU-T recomended H.264|ISO/IEC 14496-10
    | AVCVideo
    -- | 0x1C ISO/IEC 14496-3 audio(without extra transport structure(e.g. DTS, ALS, SLS))
    | Mpeg4Audio
    -- | 0x1D ISO/IEC 14496-17 text
    | Mpeg4Text
    -- | 0x1E Supplement video stream specified in ISO/IEC 23002-3
    | MpegCVideo
    -- | 0x1F SVC video subbit stream of AVC video stream based on
    -- more then one profile of ITU-T recomended H.264|ISO/IEC 14496-10 appendex.G
    | Mpeg4SVC
    -- | 0x20 MVC video subbit stream of AVC video stream based on
    -- more then one profile of ITU-T recomended H.264|ISO/IEC 14496-10 appendex.H
    | Mpeg4MVC
    -- | 0x91 Unidirectional Lightweight Encapsulation(ULE), IETF RFC 4326
    | ULE
    -- | 0xC4 - 0xFF Arib user area
    | AribUserArea Word8
    deriving (Show, Read, Eq, Ord, Typeable)

instance FromBinary StreamType where
    type BinaryRep StreamType = Word8
    fromBinary 0x01 = Mpeg1Video
    fromBinary 0x02 = Mpeg2Video
    fromBinary 0x03 = Mpeg1Audio
    fromBinary 0x04 = Mpeg2Audio
    fromBinary 0x05 = Mpeg2PrivateSection
    fromBinary 0x06 = Mpeg2PESPacket
    fromBinary 0x07 = MHEG
    fromBinary 0x08 = DSMCC
    fromBinary 0x09 = H222_1
    fromBinary 0x0A = DSMCCTypeA
    fromBinary 0x0B = DSMCCTypeB
    fromBinary 0x0C = DSMCCTypeC
    fromBinary 0x0D = DSMCCTypeD
    fromBinary 0x0E = DSMCCOtherType
    fromBinary 0x0F = Mpeg2ADTSAudio

    fromBinary 0x10 = Mpeg4Video
    fromBinary 0x11 = LATMAudio
    fromBinary 0x12 = StreamOnPESPacket
    fromBinary 0x13 = StreamOnMpeg4Section
    fromBinary 0x14 = Mpeg2SyncDownload
    fromBinary 0x15 = MetadataOnPESPacket
    fromBinary 0x16 = MetadataOnSection
    fromBinary 0x17 = MetadataOnMpeg2DataCarousel
    fromBinary 0x18 = MetadataOnMpeg2ObjectCarousel
    fromBinary 0x19 = MetadataOnMpeg2SyncDownload
    fromBinary 0x1A = IPMPStream 0x1A
    fromBinary 0x1B = AVCVideo
    fromBinary 0x1C = Mpeg4Audio
    fromBinary 0x1D = Mpeg4Text
    fromBinary 0x1E = MpegCVideo
    fromBinary 0x1F = Mpeg4SVC
    fromBinary 0x20 = Mpeg4MVC
    fromBinary 0x7F = IPMPStream 0x7F
    fromBinary 0x91 = ULE
    fromBinary w | 0xC4 <= w && w <= 0xFF = AribUserArea w
                 | otherwise              = UndefinedStreamType w

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
    | DualMono
    | VoiceExplanationForVisuallyImpaired
    | SoundForHearingImpaired 
    | OtherAudioMode Word8
    deriving (Show, Read, Eq, Ord, Typeable)

instance FromBinary AudioMode where
    type BinaryRep AudioMode = Word8
    fromBinary 0x01 = AudioMode (AudioChannel 0 0 0) (AudioChannel 1 0 0) (AudioChannel 0 0 0) 0
    fromBinary 0x02 = DualMono
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
    fromBinary 0x40 = VoiceExplanationForVisuallyImpaired
    fromBinary 0x41 = SoundForHearingImpaired
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



