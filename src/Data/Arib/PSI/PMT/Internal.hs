{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.PMT.Internal where

import Control.Applicative
import Data.Bits
import Data.Word
import Data.Binary.Get
import Data.Typeable
import Data.Tagged

import Data.Arib.PSI.Internal.Common
import Data.Arib.PSI.Descriptor.Internal

data PMT
    = PMT
        { pmtPsiHeader   :: {-#UNPACK#-}!PSIHeader
        , pmtPcrPid      :: {-#UNPACK#-}!Word16
        , pmtDescriptors :: !Descriptors
        , pmtStreams     :: ![PMTStream]
        } deriving (Show, Eq, Typeable)

data PMTStream 
    = PMTStream
        { streamType        :: StreamType
        , elementalyPID     :: Int
        , streamDescriptors :: Descriptors
        } deriving (Show, Eq, Typeable)

toStreamType :: Word8 -> StreamType
toStreamType 0x01 = Mpeg1Video
toStreamType 0x02 = Mpeg2Video
toStreamType 0x03 = Mpeg1Audio
toStreamType 0x04 = Mpeg2Audio
toStreamType 0x05 = Mpeg2PrivateSection
toStreamType 0x06 = Mpeg2PESPacket
toStreamType 0x07 = MHEG
toStreamType 0x08 = DSMCC
toStreamType 0x09 = H222_1
toStreamType 0x0A = DSMCCTypeA
toStreamType 0x0B = DSMCCTypeB
toStreamType 0x0C = DSMCCTypeC
toStreamType 0x0D = DSMCCTypeD
toStreamType 0x0E = DSMCCOtherType
toStreamType 0x0F = Mpeg2ADTSAudio

toStreamType 0x10 = Mpeg4Video
toStreamType 0x11 = LATMAudio
toStreamType 0x12 = StreamOnPESPacket
toStreamType 0x13 = StreamOnMpeg4Section
toStreamType 0x14 = Mpeg2SyncDownload
toStreamType 0x15 = MetadataOnPESPacket
toStreamType 0x16 = MetadataOnSection
toStreamType 0x17 = MetadataOnMpeg2DataCarousel
toStreamType 0x18 = MetadataOnMpeg2ObjectCarousel
toStreamType 0x19 = MetadataOnMpeg2SyncDownload
toStreamType 0x1A = IPMPStream 0x1A
toStreamType 0x1B = AVCVideo
toStreamType 0x1C = Mpeg4Audio
toStreamType 0x1D = Mpeg4Text
toStreamType 0x1E = MpegCVideo
toStreamType 0x1F = Mpeg4SVC
toStreamType 0x20 = Mpeg4MVC
toStreamType 0x7F = IPMPStream 0x7F
toStreamType 0x91 = ULE
toStreamType w | 0xC4 <= w && w <= 0xFF = AribUserArea w
               | otherwise              = UndefinedStreamType w

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
    deriving (Show, Eq, Typeable)

instance HasPSIHeader PMT where
    header = header . pmtPsiHeader
    {-# INLINE header #-}

pmt' :: (Int -> Bool) -> PSITag PMT
pmt' = Tagged

pmt :: Int -> PSITag PMT
pmt i = pmt' (== i)

-- | do nothing PSITag for PMT. (convenient to use with unWrap.)
pmt_ :: PSITag PMT
pmt_ = pmt' (const False)

instance PSI PMT where
    getPSI _ = {-# SCC "pmt'" #-} runPsi getF
      where
        getF h = do
            pcrpid <- (0x1FFF .&.) <$> getWord16be
            prglen <- fromIntegral . ( 0xFFF .&.) <$> getWord16be
            desc1  <- getDescriptors prglen
            let remain = sectionLength h - fromIntegral prglen - 13
            PMT h pcrpid desc1 <$> {-# SCC "pmt'[stream]" #-} loop remain
        loop n
            | n <= 0    = return []
            | otherwise = do
                s     <- toStreamType <$> getWord8
                epid  <- fromIntegral . (0x1FFF .&.) <$> getWord16be
                eslen <- fromIntegral . (0x0FFF .&.) <$> getWord16be
                descs <- getDescriptors (fromIntegral eslen)
                (PMTStream s epid descs:) <$> loop (n - 5 - eslen)
    {-# INLINE getPSI #-}
