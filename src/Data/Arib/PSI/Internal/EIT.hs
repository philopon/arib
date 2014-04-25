{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.Internal.EIT where

import Control.Applicative
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap.Strict as IM
import Data.Bits
import Data.Word
import Data.Binary.Get
import Data.Time

import Data.Arib.PSI.Internal.Common
import Data.Arib.PSI.Internal.Descriptor

data EIT
    = EIT 
        { eitPsiHeader             :: {-#UNPACK#-}!PSIHeader
        , eitTransportStreamId     :: {-#UNPACK#-}!Word16
        , eitOriginalNetworkId     :: {-#UNPACK#-}!Word16
        , segmentLastSectionNumber :: {-#UNPACK#-}!Word8
        , lastTableId              :: {-#UNPACK#-}!Word8
        , eitEvents                :: ![Event]
        } deriving Show

data Event
    = Event 
        { eventId          :: {-#UNPACK#-}!Word16
        , eventStartTime   :: {-#UNPACK#-}!LocalTime
        , eventDuration    :: !DiffTime
        , eventLanguage    :: {-#UNPACK#-}!S.ByteString
        , eventTitle       :: !L.ByteString
        , eventDescription :: !L.ByteString
        , eventStatus      :: {-#UNPACK#-}!Word16
        , eventScramble    :: !Bool 
        , eventDescriptors :: !Descriptors
        } deriving Show

instance PSI EIT where
    header = header . eitPsiHeader

eit :: PSIFunc EIT
eit i | i `elem` [0x12,0x26,0x27] = runPsi getF
      | otherwise                 = const []
  where
    getF h = do
        tsid <- getWord16be
        onid <- getWord16be
        slsn <- getWord8
        ltid <- getWord8
        EIT h tsid onid slsn ltid <$> loop (fromIntegral $ sectionLength h - 15 :: Int)

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
            Descriptors descs <- getDescriptors len
            let ShortEventDescriptor lang title desc = case IM.lookup 0x4D descs of
                    Nothing -> ShortEventDescriptor S.empty L.empty L.empty
                    Just e  -> e

            (Event eid (LocalTime mjd tod) (timeOfDayToTime dur) lang title desc
                stt scr (Descriptors $ IM.delete 0x4D descs) :) <$> loop (n - len - 12)
    bcd s w = shiftR w s .&. 0xf

