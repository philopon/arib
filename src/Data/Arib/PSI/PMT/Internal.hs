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
        }
        deriving (Show, Typeable)

data PMTStream 
    = PMTStream
        { streamTypeId :: Word8
        , elementalyPID :: Int
        , streamDescriptors :: Descriptors
        } deriving (Show, Typeable)

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
                s     <- fromIntegral <$> getWord8
                epid  <- fromIntegral . (0x1FFF .&.) <$> getWord16be
                eslen <- fromIntegral . (0x0FFF .&.) <$> getWord16be
                descs <- getDescriptors (fromIntegral eslen)
                (PMTStream s epid descs:) <$> loop (n - 5 - eslen)
    {-# INLINE getPSI #-}
