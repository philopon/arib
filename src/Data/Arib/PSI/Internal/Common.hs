{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.Internal.Common where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Word
import Data.Binary.Get
import Data.Typeable
import Data.Tagged

import Data.Arib.CRC

class HasPSIHeader a where
  header        :: a -> Word64
  tableId       :: a -> Word8
  tableId a = fromIntegral $ shiftR (header a) 56
  sectionLength :: a -> Int
  sectionLength a = fromIntegral $ shiftR (header a) 40 .&. 0xFFF
  versionNumber :: a -> Word8
  versionNumber a = fromIntegral $ shiftR (header a) 17 .&. 0x1F
  currentNextIndicator :: a -> Bool
  currentNextIndicator a = testBit (header a) 16
  sectionNumber :: a -> Word8
  sectionNumber a = fromIntegral $ shiftR (header a) 8
  lastSectionNumber :: a -> Word8
  lastSectionNumber a = fromIntegral $ header a

  {-# INLINE tableId #-}
  {-# INLINE sectionLength #-}
  {-# INLINE versionNumber #-}
  {-# INLINE currentNextIndicator #-}
  {-# INLINE sectionNumber #-}
  {-# INLINE lastSectionNumber #-}

checkAndGetHeader :: Get PSIHeader
checkAndGetHeader = {-# SCC "checkAndGetHeader" #-} do
    {-# SCC "checkAndGetHeader[check]" #-} lookAhead $ do
        [h,u,l] <- S.unpack <$> getByteString 3
        let len = shiftL (fromIntegral u .&. 0xf) 8 .|. fromIntegral l
        s <- getLazyByteString len
        unless (crc32 (h `L.cons` u `L.cons` l `L.cons` s) == 0) $ fail "CRC32 check failed."
    {-# SCC "checkAndGetHeader[get]" #-} PSIHeader <$> getWord64be
{-# INLINE checkAndGetHeader #-}

runPsi :: (PSIHeader -> Get a) -> L.ByteString -> [a]
runPsi get = {-# SCC "runPsi" #-} go
  where
    go s = case runGetOrFail ((checkAndGetHeader >>= get) <* skip 4) s of
        Left _ -> []
        Right (s',_,p) | L.null s'         -> [p]
                       | L.head s' == 0xFF -> [p]
                       | otherwise         -> p : go s'
{-# INLINE runPsi #-}

newtype PSIHeader = PSIHeader Word64
instance Show PSIHeader where
    show h = "PSIHeader {tableId = "     ++ show (tableId h) ++
             ", sectionLength = "        ++ show (sectionLength h) ++
             ", versionNumber = "        ++ show (versionNumber h) ++
             ", currentNextIndicator = " ++ show (currentNextIndicator h) ++
             ", sectionNumber = "        ++ show (sectionNumber h) ++
             ", lastSectionNumber = "    ++ show (lastSectionNumber h) ++
             "}"

instance HasPSIHeader PSIHeader where
    header (PSIHeader h) = h
    {-# INLINE header #-}

instance HasPSIHeader Word64 where
    header = id
    {-# INLINE header #-}

type PSITag a = Tagged a (Int -> Bool)

class (Typeable a, Show a) => PSI a where
  getPSI   :: PSITag a  -> L.ByteString -> [a]

raw :: PSITag L.ByteString
raw = Tagged (const True)
{-# INLINE raw #-}

instance PSI L.ByteString where
    getPSI   _   = (:[])
    {-# INLINE getPSI #-}

