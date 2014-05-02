{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Arib.PSI.Descriptor.Internal where

import Control.Applicative
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import qualified Data.Text.Lazy as TL

import Data.Word
import Data.Binary.Get

import Data.Arib.String

-- | B-10 Table.6-5 (pp. 122-126 pdf pp. 134-138)
newtype ComponentTag = ComponentTag Word8 deriving Show

data ShortEvent = ShortEvent
    { shortEventLanguage    :: {-# UNPACK #-}!S.ByteString
    , shortEventTitle       :: TL.Text
    , shortEventDescription :: TL.Text
    } deriving Show

data Descriptors_ streamId shortEvent other
    = Descriptors
        { 
        -- | 0x52
          streamId   :: streamId
        -- | 0x4D
        , shortEvent :: shortEvent
        , other      :: other 
        } deriving Show
type Descriptors = Descriptors_ [ComponentTag] [ShortEvent] [(Word8, L.ByteString)]

type GetDesc a = Get (Word8, [a] -> [a])

getStreamId :: GetDesc ComponentTag
getStreamId = skip 1 >> getWord8 >>= \w -> return (3, (:) $ ComponentTag w)

getShortEvent :: GetDesc ShortEvent
getShortEvent = do
    len   <- getWord8
    lang  <- getByteString 3
    pLen  <- fromIntegral <$> getWord8
    title <- decodeText =<< getLazyByteString pLen
    dLen  <- fromIntegral <$> getWord8
    desc  <- decodeText =<< getLazyByteString dLen
    return (len + 2, (:) $ ShortEvent lang title desc )

getRaw :: Word8 -> GetDesc (Word8, L.ByteString)
getRaw w = getWord8 >>= \len -> getLazyByteString (fromIntegral len) >>= \s -> return (len + 2, (:) (w, s))

getDescriptors :: Int -> Get Descriptors
getDescriptors s = reduceDesc <$> go (Descriptors id id id) s
  where
    reduceDesc (Descriptors a b c) = Descriptors (a []) (b []) (c [])
    go descs len
        | len <= 0  = return descs
        | otherwise = getWord8 >>= \case
            0x52 -> getStreamId   >>= \(l, f) -> go (descs { streamId   = streamId   descs . f }) (len - fromIntegral l)
            0x4D -> getShortEvent >>= \(l, f) -> go (descs { shortEvent = shortEvent descs . f }) (len - fromIntegral l)
            w    -> getRaw w      >>= \(l, f) -> go (descs { other      = other      descs . f }) (len - fromIntegral l)

