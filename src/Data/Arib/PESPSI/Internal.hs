{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Arib.PESPSI.Internal where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as B
import Data.Conduit
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import Data.Typeable
import Data.Bits
import Data.Word
import Data.List
import Data.Maybe
import Data.Binary.Get

import Data.Arib.TS.Internal

data PESPSI a
    = PESPSI
        { pesPsiProgramId       :: {-#UNPACK#-}!Int
        , pesPsiAdaptationField :: {-#UNPACK#-}!L.ByteString
        , pesPsiPayload         :: a
        } deriving (Show, Functor, Typeable)

concatTsPackets' :: Monad m 
                 => (TS S.ByteString -> ConduitM (TS S.ByteString) o m ())
                 -> (PESPSI L.ByteString -> ConduitM (TS S.ByteString) o m ())
                 -> ConduitM (TS S.ByteString) o m ()
concatTsPackets' scrambledF unscrambledF = go IM.empty
  where
    go dict = await >>= \case
        Just ts
            | transportErrorIndicator ts           -> go dict
            | tsTransportScramblingControl ts /= 0 -> scrambledF ts >> go dict
            | otherwise -> case IM.lookup (tsProgramId ts) dict of
                Just acc 
                    -- Continued Packet
                    | ts `isNextOf` acc && not (payloadUnitStartIndicator ts) ->
                        go $ IM.insert (tsProgramId ts) (acc <> fmap B.byteString ts) dict

                    -- Start Packet -> yield chunked packet
                    | payloadUnitStartIndicator ts -> do
                        let (ad, pl) = splitPayload (hasAdaptationField acc) (hasPayload acc)
                                       (B.toLazyByteString $ tsPayload acc)
                        unscrambledF $ PESPSI (tsProgramId acc) ad pl
                        go $ IM.insert (tsProgramId ts) (fmap B.byteString ts) dict
                    | otherwise -> go dict -- !payloadunitstart && !next packet

                Nothing
                    -- Start Packet
                    | payloadUnitStartIndicator ts -> 
                        go $ IM.insert (tsProgramId ts) (fmap B.byteString ts) dict
                    | otherwise -> go dict
        Nothing -> return ()

    splitPayload adaptation payload b =
        let (a,b') = if adaptation
                         then L.splitAt (fromIntegral (L.head b)) (L.tail b)
                         else (L.empty, b)
            p      = if payload 
                         then L.drop (fromIntegral (L.head b')) (L.tail b')
                         else L.empty
        in (a,p)

concatTsPackets :: Monad m => Conduit (TS S.ByteString) m (Either (TS S.ByteString) (PESPSI L.ByteString))
concatTsPackets = concatTsPackets' (yield . Left) (yield . Right)

concatTsPackets_ :: Monad m => Conduit (TS S.ByteString) m (PESPSI L.ByteString)
concatTsPackets_ = concatTsPackets' (const $ return ()) yield

data Wrapper = forall a. (Typeable a, Show a) => Wrap a
deriving instance Show Wrapper

class PSI a where
  header        :: a -> Word64
  sectionLength :: a -> Int
  sectionLength a = fromIntegral $ shiftR (header a) 40 .&. 0xFFF
  {-# INLINE sectionLength #-}

psiHeader :: (Word64 -> L.ByteString -> a) -> L.ByteString -> a
psiHeader f s = case runGetOrFail getWord64be s of
    Left  (_,_,e) -> error $ "psiHeader: " ++ e
    Right (o,_,w) -> f w o

data PAT 
    = PAT
        { patPsiHeader  :: {-#UNPACK#-}!Word64
        , programPidMap :: {-#UNPACK#-}!(IM.IntMap Int)
        } deriving(Show, Typeable)

instance PSI PAT where
    header = patPsiHeader
    {-# INLINE header #-}

instance PSI Word64 where
    header = id
    {-# INLINE header #-}

pat :: Int -> L.ByteString -> Maybe PAT
pat 0 bs = Just $ psiHeader go bs
  where
    go h b =
        let r = runGet (replicateM ((sectionLength h - 9) `quot` 4) ((,) <$> getWord16be <*> getWord16be)) b
            in PAT h $ foldl' foldF IM.empty r
    foldF m (0,_) = m
    foldF m (u,l) = IM.insert (fromIntegral u) (fromIntegral l .&. 0x1FFF) m
pat _ _  = Nothing

type WrappedFunc = PESPSI L.ByteString -> Maybe Wrapper

wrap :: (Typeable a, Show a) => (Int -> L.ByteString -> Maybe a) -> WrappedFunc
wrap f p@PESPSI{..} = (\b -> Wrap $ p {pesPsiPayload = b}) <$> f pesPsiProgramId pesPsiPayload 

multiPESPSI :: Monad m => [WrappedFunc] -> Conduit (PESPSI L.ByteString) m Wrapper
multiPESPSI fs = awaitForever $ \raw ->
     case mapMaybe (\f -> f raw) fs of
         []  -> return ()
         a:_ -> yield a

singlePESPSI :: Monad m => (Int -> a -> Maybe b) -> Conduit (PESPSI a) m (PESPSI b)
singlePESPSI f = awaitForever $ \raw ->
    case f (pesPsiProgramId raw) (pesPsiPayload raw) of
        Nothing -> return ()
        Just a  -> yield raw{pesPsiPayload = a}

