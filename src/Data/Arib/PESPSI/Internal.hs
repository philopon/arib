{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PESPSI.Internal where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as B
import Data.Conduit
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import Data.Typeable
import Data.Word

import Data.Arib.TS

data PESPSI a
    = PESPSI
        { pesPsiProgramId       :: {-#UNPACK#-}!Int
        , pesPsiAdaptationField :: !L.ByteString
        , pesPsiPayload         :: !a
        } deriving (Show, Functor, Typeable)

isNextOf' :: Word8 -> Word8 -> Bool
0x0 `isNextOf'` 0xf = True
0x0 `isNextOf'` _   = False
a   `isNextOf'` b   = succ b == a
{-# INLINE isNextOf' #-}

isNextOf :: TS -> Word8 -> Bool
isNextOf ts w = not (discontinuityIndicator ts) || isNextOf' (continuityCounter ts) w
{-# INLINE isNextOf #-}

appendPacket :: Monoid a => PESPSI a -> a -> PESPSI a
PESPSI pid ad ab `appendPacket` bs = PESPSI pid ad (ab <> bs)
{-# INLINE appendPacket #-}

concatTsPackets' :: Monad m 
                 => (TS -> ConduitM TS o m ())
                 -> (PESPSI L.ByteString -> ConduitM TS o m ())
                 -> ConduitM TS o m ()
concatTsPackets' scrambledF unscrambledF =
    {-# SCC "concatTsPackets'" #-} go (IM.empty :: IM.IntMap (Word8, PESPSI B.Builder))
  where
    go dict = await >>= \case
        Just ts
            | transportErrorIndicator ts           -> go dict
            | tsTransportScramblingControl ts /= 0 -> scrambledF ts >> go dict
            | otherwise -> case IM.lookup (tsProgramId ts) dict of
                Just (cc, acc)
                    -- Continued Packet
                    | ts `isNextOf` cc && not (payloadUnitStartIndicator ts) ->
                        {-# SCC "concatTsPackets'[ContinuedPacket]" #-}
                        go $ IM.insert (tsProgramId ts) 
                        (continuityCounter ts, acc `appendPacket` B.byteString (tsPayload ts)) dict

                    -- Start Packet -> yield chunked packet
                    | payloadUnitStartIndicator ts -> {-# SCC "concatTsPackets'[StartAndYieldPacket]" #-} do
                        let (ad, bf, pl) = splitPayload (hasAdaptationField ts) (hasPayload ts) (tsPayload ts)
                        unscrambledF . fmap B.toLazyByteString $ acc `appendPacket` bf
                        go $ IM.insert (tsProgramId ts) (continuityCounter ts, PESPSI (tsProgramId ts) ad pl) dict

                    -- !payloadunitstart && !next packet
                    | otherwise -> go dict 
                Nothing
                    | payloadUnitStartIndicator ts -> {-# SCC "concatTsPackets'[StartPacket]" #-} do
                        let (ad, _, pl) = splitPayload (hasAdaptationField ts) (hasPayload ts) (tsPayload ts)
                        go $ IM.insert (tsProgramId ts) (continuityCounter ts, PESPSI (tsProgramId ts) ad pl) dict
                    | otherwise -> go dict
        Nothing -> return ()
    splitPayload adaptation payload b =
        let (a,b') = if adaptation
                         then S.splitAt (fromIntegral (S.head b)) (S.tail b)
                         else (S.empty, b)
            (bf,p) = if payload 
                         then S.splitAt (fromIntegral (S.head b')) (S.tail b')
                         else (b', S.empty)
        in (L.fromStrict a,B.byteString bf,B.byteString p)
{-# INLINE concatTsPackets' #-}

concatTsPackets :: Monad m => Conduit TS m (Either TS (PESPSI L.ByteString))
concatTsPackets = concatTsPackets' (yield . Left) (yield . Right)
{-# INLINE concatTsPackets #-}

concatTsPackets_ :: Monad m => Conduit TS m (PESPSI L.ByteString)
concatTsPackets_ = concatTsPackets' (const $ return ()) yield
{-# INLINE concatTsPackets_ #-}

