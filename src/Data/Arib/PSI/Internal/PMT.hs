{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.Internal.PMT where

import Control.Applicative
import Data.Bits
import Data.Word
import Data.Binary.Get

import Data.Arib.PSI.Internal.Common
import Data.Arib.PSI.Internal.Descriptor

data PMT
    = PMT
        { pmtPsiHeader   :: {-#UNPACK#-}!PSIHeader
        , pmtPcrPid      :: {-#UNPACK#-}!Word16
        , pmtDescriptors :: !Descriptors
        , pmtStreams     :: ![PMTStream]
        }
        deriving Show

data PMTStream 
    = PMTStream
        { streamTypeId :: Word8
        , elementalyPID :: Int
        , streamDescriptors :: Descriptors
        } deriving Show

instance PSI PMT where
    header = header . pmtPsiHeader
    {-# INLINE header #-}

pmt :: Int -> PSIFunc PMT
pmt i = pmt' (== i)
{-# INLINE pmt #-}

pmt' :: (Int -> Bool) -> PSIFunc PMT
pmt' f i | f i      = {-# SCC "pmt'" #-} runPsi getF
        | otherwise = const []
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
{-# INLINE pmt' #-}
