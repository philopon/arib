{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Binary.Get

import Data.Arib.TS.Internal
import Data.Arib.CRC

data PESPSI a
    = PESPSI
        { pesPsiProgramId       :: {-#UNPACK#-}!Int
        , pesPsiAdaptationField :: {-#UNPACK#-}!L.ByteString
        , pesPsiPayload         :: a
        } deriving (Show, Functor, Typeable)

isNextOf :: Word8 -> Word8 -> Bool
0x0 `isNextOf` 0xf = True
0x0 `isNextOf` _   = False
a   `isNextOf` b   = succ b == a

appendPacket :: Monoid a => PESPSI a -> a -> PESPSI a
PESPSI pid ad ab `appendPacket` bs = PESPSI pid ad (ab <> bs)

concatTsPackets' :: Monad m 
                 => (TS S.ByteString -> ConduitM (TS S.ByteString) o m ())
                 -> (PESPSI L.ByteString -> ConduitM (TS S.ByteString) o m ())
                 -> ConduitM (TS S.ByteString) o m ()
concatTsPackets' scrambledF unscrambledF = go (IM.empty :: IM.IntMap (Word8, PESPSI B.Builder))
  where
    go dict = await >>= \case
        Just ts
            | transportErrorIndicator ts           -> go dict
            | tsTransportScramblingControl ts /= 0 -> scrambledF ts >> go dict
            | otherwise -> case IM.lookup (tsProgramId ts) dict of
                Just (cc, acc)
                    -- Continued Packet
                    | continuityCounter ts `isNextOf` cc && not (payloadUnitStartIndicator ts) ->
                        go $ IM.insert (tsProgramId ts) 
                        (continuityCounter ts, acc `appendPacket` B.byteString (tsPayload ts)) dict

                    -- Start Packet -> yield chunked packet
                    | payloadUnitStartIndicator ts -> do
                        let (ad, bf, pl) = splitPayload (hasAdaptationField ts) (hasPayload ts) (tsPayload ts)
                        unscrambledF . fmap B.toLazyByteString $ acc `appendPacket` bf
                        go $ IM.insert (tsProgramId ts) (continuityCounter ts, PESPSI (tsProgramId ts) ad pl) dict

                    -- !payloadunitstart && !next packet
                    | otherwise -> go dict 
                Nothing
                    | payloadUnitStartIndicator ts -> do
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

concatTsPackets :: Monad m => Conduit (TS S.ByteString) m (Either (TS S.ByteString) (PESPSI L.ByteString))
concatTsPackets = concatTsPackets' (yield . Left) (yield . Right)

concatTsPackets_ :: Monad m => Conduit (TS S.ByteString) m (PESPSI L.ByteString)
concatTsPackets_ = concatTsPackets' (const $ return ()) yield

data Wrapper = forall a. (Typeable a, Show a) => Wrap (PESPSI a)
deriving instance Show Wrapper

class PSI a where
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

pat :: PESPSIFunc PAT
pat 0 = psiHeader $ \h -> Just . runGet (getF h)
  where
    getF h = PAT h <$> foldM (\m _ -> do
        genre <- getWord16be
        pid   <- fromIntegral . (0x1FFF .&.) <$> getWord16be
        return $ if genre == 0
                 then m
                 else IM.insert (fromIntegral genre) pid m
        ) IM.empty [1 .. ((sectionLength h - 9) `quot` 4)]

pat _ = const Nothing

data PMT
    = PMT
        { pmtPsiHeader   :: {-#UNPACK#-}!Word64
        , pmtPcrPid      :: {-#UNPACK#-}!Word16
        , pmtDescriptors :: {-#UNPACK#-}!Descriptors
        , pmtStreams     :: {-#UNPACK#-}!(IM.IntMap (Word8, Descriptors))
        }
        deriving Show

pmt :: (Int -> Bool) -> PESPSIFunc PMT
pmt f i | f i       = psiHeader $ \h -> Just . runGet (getF h)
        | otherwise = const Nothing
  where
    getF h = do
        pcrpid <- (0x1FFF .&.) <$> getWord16be
        prglen <- fromIntegral . ( 0xFFF .&.) <$> getWord16be
        desc1  <- getDescriptors prglen
        let remain = sectionLength h - fromIntegral prglen - 13
        PMT h pcrpid desc1 <$> loop IM.empty remain
    loop dict n
        | n <= 0    = return dict
        | otherwise = do
            s     <- fromIntegral <$> getWord8
            epid  <- fromIntegral . (0x1FFF .&.) <$> getWord16be
            eslen <- fromIntegral . (0x0FFF .&.) <$> getWord16be
            descs <- getDescriptors (fromIntegral eslen)
            loop (IM.insert epid (s, descs) dict) (n - 5 - eslen)

data Descriptor 
    = StreamIdDescriptor Word8
    | Raw L.ByteString
    deriving Show

getDescriptor :: Get (Int, Int, Descriptor)
getDescriptor = getWord8 >>= \case
    0x52 -> getWord8 >> ((3,0x52,) . StreamIdDescriptor <$> getWord8)
    w    -> getWord8 >>= \len -> (fromIntegral $ len + 2, fromIntegral w,) . Raw <$>
            (getLazyByteString (fromIntegral len))

newtype Descriptors = Descriptors (IM.IntMap Descriptor)
                    deriving Show

streamIdDescriptor :: Descriptors -> Maybe Word8
streamIdDescriptor (Descriptors d) = IM.lookup 0x52 d >>= unWrap
  where unWrap (StreamIdDescriptor w) = Just w
        unWrap _ = Nothing

getDescriptors :: Int -> Get Descriptors
getDescriptors = go IM.empty
  where
    go dict len 
        | len <= 0  = return $ Descriptors dict
        | otherwise = getDescriptor >>= \(l,i,d) -> go (IM.insert i d dict) (len - l)

raw :: PESPSIFunc L.ByteString
raw _ = Just

type PESPSIFunc a = Int -> L.ByteString -> Maybe a
type WrappedFunc  = PESPSI L.ByteString -> Maybe Wrapper

wrap :: (Typeable a, Show a) => (PESPSIFunc a) -> WrappedFunc
wrap f p@PESPSI{..} = (\b -> Wrap $ p {pesPsiPayload = b}) <$> f pesPsiProgramId pesPsiPayload 

data WrappedFuncs = forall a. (Typeable a, Show a) => (Int -> L.ByteString -> Maybe a) :-> WrappedFuncs
                  | END 
infixr :->
infixr -|

(-|) :: (Typeable a, Typeable b, Show a, Show b) => PESPSIFunc a -> PESPSIFunc b -> WrappedFuncs
a -| b = a :-> b :-> END

multiPESPSI :: Monad m => WrappedFuncs -> Conduit (PESPSI L.ByteString) m Wrapper
multiPESPSI = awaitForever . go
  where
    go END        _ = return ()
    go (f :-> fs) r = maybe (go fs r) (yield . Wrap . (\p -> r{pesPsiPayload = p})) $
                      f (pesPsiProgramId r) (pesPsiPayload r)

singlePESPSI :: Monad m => PESPSIFunc a -> Conduit (PESPSI L.ByteString) m (PESPSI a)
singlePESPSI f = awaitForever $ \r ->
    case f (pesPsiProgramId r) (pesPsiPayload r) of
        Nothing -> return ()
        Just a  -> yield r{pesPsiPayload = a}

