{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
-- module Data.Arib.TS.Internal where

import Control.Monad
import Control.Exception
import Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word
import Data.Function
import Data.List
import Data.Bits
import qualified Data.IntMap.Strict as IM
import Data.Typeable(Typeable)

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL

data TS = 
    TS { header1 :: {-#UNPACK#-}!Word8
       , header2 :: {-#UNPACK#-}!Word8
       , header3 :: {-#UNPACK#-}!Word8
       , payload :: {-#UNPACK#-}!S.ByteString
       } deriving Show

transportErrorIndicator, payloadUnitStartIndicator, transportPriority :: TS -> Bool
transportErrorIndicator   TS{header1}  = testBit header1 7
payloadUnitStartIndicator TS{header1}  = testBit header1 6
transportPriority         TS{header1}  = testBit header1 5
programId :: TS -> Int
programId TS{header1, header2} =
    shiftL (fromIntegral $ header1 .&. 0x1F) 8 .&. fromIntegral header2
transportScramblingControl, adaptationFieldControl, continuityCounter :: TS -> Word8
transportScramblingControl TS{header3} = shiftR header3 6
adaptationFieldControl     TS{header3} = shiftR header3 4 .&. 0x3
continuityCounter          TS{header3} = header3 .&. 0xF

data TsException
    = ReSyncFailed
    deriving (Show,Typeable)
instance Exception TsException

ts :: MonadThrow m => Int -> Conduit S.ByteString m TS
ts n = go S.empty
  where
    n64 :: Int64
    n64 = fromIntegral n

    go beforePayload = do
        rawPacket <- CB.take n
        when (L.length rawPacket == n64) $ do
            packet <- resync n64 beforePayload rawPacket

            let (h,p) = L.splitAt 4 packet
            let [_,h1,h2,h3] = L.unpack h

            yield $ TS h1 h2 h3 (L.toStrict p)
            go (L.toStrict p)

safeIndexL :: L.ByteString -> Int64 -> Maybe Word8
safeIndexL b n
    | n < 0           = Nothing
    | n >= L.length b = Nothing
    | otherwise       = Just $ L.index b n

safeIndexS :: S.ByteString -> Int -> Maybe Word8
safeIndexS b n
    | n < 0           = Nothing
    | n >= S.length b = Nothing
    | otherwise       = Just $ S.index b n

resync :: MonadThrow m
       => Int64 -> S.ByteString -> L.ByteString -> ConduitM S.ByteString a m L.ByteString
resync len bp rp
    | L.head rp == 0x47 = return rp
    | otherwise         = go 1
  where
    bpLen = S.length bp
    go i
        | safeIndexS bp (bpLen - i) == Just 0x47 = do
            let (res,lo) = L.splitAt len $ L.fromStrict (S.drop (bpLen - i) bp) `L.append` rp
            leftover (L.toStrict lo)
            return res

        | safeIndexL rp (fromIntegral i - 1) == Just 0x47 =
            (L.drop (fromIntegral i - 1) rp `L.append`) <$> CB.take (i - 1)

        | otherwise = if len > fromIntegral i then go (i + 1) else monadThrow ReSyncFailed

detectPacketSize :: Monad m => Consumer S.ByteString m Int
detectPacketSize = CL.peek >>= \case
    Nothing -> return 0
    Just c  -> do
        let m = foldl' (\i a -> IM.insertWith (const succ) (fromIntegral $ S.length a) 1 i) IM.empty $ S.split 0x47 c
        return . succ . fst . maximumBy (compare `on` snd) $ IM.toList (m :: IM.IntMap Int)

main :: IO ()
main = do
    a <- runResourceT $ CB.sourceFile "../test.ts" $$ ts 188 =$ CL.fold (\a _ -> a + 1) (0::Int)
    print a

