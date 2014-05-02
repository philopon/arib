{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.EIT.Internal where

import Control.Applicative
import Data.Bits
import Data.Word
import Data.Binary.Get
import Data.Time
import Data.Tagged
import Data.Typeable

import Data.Arib.PSI.Internal.Common
import Data.Arib.PSI.Descriptor.Internal

data EIT
    = EIT 
        { eitPsiHeader             :: {-#UNPACK#-}!PSIHeader
        , eitTransportStreamId     :: {-#UNPACK#-}!Word16
        , eitOriginalNetworkId     :: {-#UNPACK#-}!Word16
        , segmentLastSectionNumber :: {-#UNPACK#-}!Word8
        , lastTableId              :: {-#UNPACK#-}!Word8
        , eitEvents                :: ![Event]
        } deriving (Show, Typeable)

data Event
    = Event 
        { eventId          :: {-#UNPACK#-}!Word16
        , eventStartTime   :: {-#UNPACK#-}!LocalTime
        , eventDuration    :: !DiffTime
        , eventStatus      :: {-#UNPACK#-}!Word16
        , eventScramble    :: !Bool 
        , eventDescriptors :: !Descriptors
        } deriving (Show, Typeable)

instance HasPSIHeader EIT where
    header = header . eitPsiHeader
    {-# INLINE header #-}

eit :: PSITag EIT
eit = Tagged isTarget
  where
    isTarget i | i  == 0x12 || i == 0x26 || i == 0x27 = True
               | otherwise                            = False


instance PSI EIT where
    getPSI _ = {-# SCC "eit" #-} runPsi getF
      where
        getF h = do
            tsid <- getWord16be
            onid <- getWord16be
            slsn <- getWord8
            ltid <- getWord8
            EIT h tsid onid slsn ltid <$> {-# SCC "eit[events]" #-} loop (fromIntegral $ sectionLength h - 15 :: Int)
          where 
            loop n 
                | n <= 0    = return []
                | otherwise = do
                    eid <- getWord16be
                    tim <- getWord64be
                    let mjd = ModifiedJulianDay . fromIntegral $ shiftR tim 48
                        tod = TimeOfDay (fromIntegral $ bcd 40 tim) (fromIntegral $ bcd 32 tim) (fromIntegral $ bcd 24 tim)
                        dur = TimeOfDay (fromIntegral $ bcd 16 tim) (fromIntegral $ bcd  8 tim) (fromIntegral $ bcd  0 tim)
                    st  <- getWord16be
                    let stt = shiftR  st 13
                        scr = testBit st 12
                        len = fromIntegral $ 0xFFF .&. st
                    descs <- getDescriptors len
                    (Event eid (LocalTime mjd tod) (timeOfDayToTime dur) stt scr descs :) <$> loop (n - len - 12)
            bcd s w = shiftR w s .&. 0xf
    {-# INLINE getPSI #-}

