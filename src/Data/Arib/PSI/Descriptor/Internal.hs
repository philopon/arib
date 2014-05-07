{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Arib.PSI.Descriptor.Component
import Data.Arib.PSI.Descriptor.Content

--------------------------------------------------------------------------------

data ShortEvent = ShortEvent
    { shortEventLanguage    :: {-# UNPACK #-}!S.ByteString
    , shortEventTitle       :: TL.Text
    , shortEventDescription :: TL.Text
    } deriving (Show, Read, Eq, Ord, Typeable)

--------------------------------------------------------------------------------

data ExtendedEvent a = ExtendedEvent
    { extendedEventLanguage :: {-# UNPACK #-}!S.ByteString
    , extendedEventItems    :: [(TL.Text, a)]
    , extendedEventText     :: a
    } deriving (Show, Read, Eq, Ord, Typeable)

reduceExtendedEvent :: Monad m => [(Word8, Word8, ExtendedEvent L.ByteString)] -> m [ExtendedEvent TL.Text]
reduceExtendedEvent = go "" [] "" 0 . dropWhile (\(w,_,_) -> w /= 0)
  where
    go _ _ _ _ [] = return []
    go lng is ts m ((n,ln,ExtendedEvent l i t):es)
        | m == n && n == ln = do
            items <- mapM (\(a,b) -> (a,) `liftM` toText b) . concatItem $ is ++ i
            text  <- toText $ ts `L.append` t
            (ExtendedEvent l items text :) `liftM` go l [] "" 0 es
        | m == n            = go l (is ++ i) (ts `L.append` t) (succ m) es
        | otherwise         = go lng is ts m es
    concatItem []  = []
    concatItem [a] = [a]
    concatItem ((at,ab):(bt,bb):is)
        | TL.null bt = concatItem $ (at,ab `L.append` bb):is
        | otherwise  = (at,ab) : concatItem ((bt,bb):is)

reduceExtendedEvent' :: [(Word8, Word8, ExtendedEvent TL.Text)] -> [ExtendedEvent TL.Text]
reduceExtendedEvent' = go "" [] "" 0 . dropWhile (\(w,_,_) -> w /= 0)
  where
    go _ _ _ _ [] = []
    go lng is ts m ((n,ln,ExtendedEvent l i t):es)
        | m == n && n == ln = ExtendedEvent l (concatItem $ is ++ i) (ts `TL.append` t) : go l [] "" 0 es
        | m == n            = go l (is ++ i) (ts `TL.append` t) (succ m) es
        | otherwise         = go lng is ts m es
    concatItem []  = []
    concatItem [a] = [a]
    concatItem ((at,ab):(bt,bb):is)
        | TL.null bt = concatItem $ (at,ab `TL.append` bb):is
        | otherwise  = (at,ab) : concatItem ((bt,bb):is)


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
        -- | 0x4E
        , extendedEvent      :: [ExtendedEvent TL.Text]
        -- | 0x50
        , component          :: [Component]
        -- | 0x52
        , streamId           :: [StreamId]
        -- | 0x54
        , content            :: [Content]
        -- | 0xC4
        , audioComponent     :: [AudioComponent]
        -- | 0xC8
        , videoDecodeControl :: [VideoDecodeControl]
        -- | 0xD6
        , eventGroup         :: [EventGroup]
        , other              :: [(Word8, L.ByteString)]
        } deriving (Show, Read, Eq, Ord, Typeable)

data Descriptors_ 
    = Descriptors_
        { shortEvent_         :: [ShortEvent]                    -> [ShortEvent]
        , extendedEvent_      :: [(Word8, Word8, ExtendedEvent L.ByteString)]
                              -> [(Word8, Word8, ExtendedEvent L.ByteString)]
        , component_          :: [Component]                     -> [Component]
        , streamId_           :: [StreamId]                      -> [StreamId]
        , content_            :: [Content]                       -> [Content]
        , audioComponent_     :: [AudioComponent]                -> [AudioComponent]
        , videoDecodeControl_ :: [VideoDecodeControl]            -> [VideoDecodeControl]
        , eventGroup_         :: [EventGroup]                    -> [EventGroup]
        , other_              :: [(Word8, L.ByteString)]         -> [(Word8, L.ByteString)]
        }

toText :: Monad m => L.ByteString -> m TL.Text
toText = either (fail . show) return . decodeText

getDescriptor :: Word8 -> Word8 -> Descriptors_ -> Get Descriptors_
getDescriptor 0x4D _ descs = do
    lang  <- getByteString 3
    pLen  <- fromIntegral <$> getWord8
    title <- toText =<< getLazyByteString pLen
    dLen  <- fromIntegral <$> getWord8
    desc  <- toText =<< getLazyByteString dLen
    return $ descs { shortEvent_ = shortEvent_ descs . (:) (ShortEvent lang title desc) }

getDescriptor 0x4E _ descs = do
    (dn, ldn) <- getWord8 >>= \w -> return (shiftR w 4, w .&. 0xF)
    lang      <- getByteString 3
    loi       <- fromIntegral <$> getWord8
    is        <- reverse <$> getItems loi
    tl        <- fromIntegral <$> getWord8
    t         <- getLazyByteString tl
    return $ descs { extendedEvent_ = extendedEvent_ descs . (:) (dn, ldn, ExtendedEvent lang is t) }
  where
    getItems len
        | len <= 0  = return []
        | otherwise = do
            idl  <- fromIntegral <$> getWord8
            idsc <- toText =<< getLazyByteString idl
            il   <- fromIntegral <$> getWord8
            i    <- getLazyByteString il
            (:) (idsc, i) <$> getItems (len - idl - il - 2)

getDescriptor 0x50 len descs = do
    sc   <- (0xF .&.) <$> getWord8
    cty  <- getWord8
    ctg  <- ComponentTag <$> getWord8
    lang <- getByteString 3
    desc <- toText =<< getLazyByteString (fromIntegral len - 6)
    return $ descs { component_ = component_ descs . (:) (Component (curry fromBinary sc cty) ctg lang desc) }

getDescriptor 0x52 _ descs = getWord8 >>= \w ->
    return $ descs { streamId_     = streamId_     descs . (ComponentTag   w:) }

getDescriptor 0x54 _ descs = do
    genre <- fromBinary <$> getWord8
    un    <- getWord8
    return $ descs { content_ = content_ descs . (Content genre un:) }

getDescriptor 0xC4 len descs = do
    cty <- skip 1 >> fromBinary <$> getWord8
    ctg <- ComponentTag <$> getWord8
    sty <- fromBinary <$> getWord8
    sc  <- getWord8 >>= \w -> return (if w == 0xFF then Nothing else Just w)
    fs  <- getWord8
    let ml = if cty == DualMono then testBit fs 7 else False
        mc = testBit fs 6
        qi = shiftR fs 4 .&. 0x3
        sr = case shiftR fs 1 .&. 0x7 of
                1 -> 16000
                2 -> 22050
                3 -> 24000
                5 -> 32000
                6 -> 44100
                7 -> 48000
                _ -> 0
    l1  <- getByteString 3
    l2  <- if ml then Just <$> getByteString 3 else return Nothing
    d   <- toText =<< getLazyByteString (fromIntegral len - 11 - if ml then 3 else 0)
    return $ descs { audioComponent_ = audioComponent_ descs . (:) (AudioComponent cty ctg sty sc ml mc qi sr l1 l2 d) }

getDescriptor 0xC8 _ descs = getWord8 >>= \w -> 
    return $ descs { videoDecodeControl_ = videoDecodeControl_ descs . (fromBinary w:) }

getDescriptor 0xD6 len descs = do
    gtec <- getWord8
    sem  <- replicateM (fromIntegral $ gtec .&. 0xF) ((,) <$> getWord16be <*> getWord16be)
    pd   <- getLazyByteString . fromIntegral $ len - 1 - (gtec .&. 0xF) * 4
    return $ descs { eventGroup_ = eventGroup_ descs . (:) (EventGroup (fromBinary $ shiftR gtec 4) sem pd) }

getDescriptor w    len descs = do
    s   <- getLazyByteString (fromIntegral len)
    return $ descs { other_ = other_ descs . (:) (w, s) }

getDescriptors :: Int -> Get Descriptors
getDescriptors s = reduceDesc =<< go (Descriptors_ id id id id id id id id id) s
  where
    reduceDesc (Descriptors_ a ee b c d e f g h) = do
        ee' <- reduceExtendedEvent $ ee []
        return $ Descriptors (a []) ee' (b []) (c []) (d []) (e []) (f []) (g []) (h [])
    go descs len
        | len <= 0  = return descs
        | otherwise = do
            idesc <- getWord8
            len'  <- getWord8
            getDescriptor idesc len' descs >>= \descs' -> go descs' (len - fromIntegral len' - 2)
