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
import Data.Time

import Data.Arib.String.Internal
import Data.Arib.TS.Internal
import Data.Arib.CRC

import Debug.Trace

data PESPSI a
    = PESPSI
        { pesPsiProgramId       :: {-#UNPACK#-}!Int
        , pesPsiAdaptationField :: !L.ByteString
        , pesPsiPayload         :: !a
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

checkAndGetHeader :: Get PSIHeader
checkAndGetHeader = do
    lookAhead $ do
        [h,u,l] <- S.unpack <$> getByteString 3
        let len = shiftL (fromIntegral u .&. 0xf) 8 .|. fromIntegral l
        s <- getLazyByteString len
        unless (crc32 (h `L.cons` u `L.cons` l `L.cons` s) == 0) $ fail "CRC32 check failed."
    PSIHeader <$> getWord64be

runPsi :: (PSIHeader -> Get a) -> L.ByteString -> [a]
runPsi get = go
  where
    go s = case runGetOrFail ((checkAndGetHeader >>= get) <* skip 4) s of
        Left _ -> traceShow "error" []
        Right (s',_,p) | L.null s'         -> [p]
                       | L.head s' == 0xFF -> [p]
                       | otherwise         -> p : go s'

data PAT 
    = PAT
        { patPsiHeader  :: {-#UNPACK#-}!PSIHeader
        , programPidMap :: !(IM.IntMap Int)
        } deriving(Show, Typeable)

instance PSI PAT where
    header = header . patPsiHeader
    {-# INLINE header #-}

newtype PSIHeader = PSIHeader Word64
instance Show PSIHeader where
    show h = "PSIHeader {tableId = " ++ show (tableId h) ++
             ", sectionLength = " ++ show (sectionLength h) ++
             ", versionNumber = "          ++ show (versionNumber h) ++
             ", currentNextIndicator = " ++ show (currentNextIndicator h) ++
             ", sectionNumber = " ++ show (sectionNumber h) ++
             ", lastSectionNumber = " ++ show (lastSectionNumber h) ++
             "}"

instance PSI PSIHeader where
    header (PSIHeader h) = h
    {-# INLINE header #-}

instance PSI Word64 where
    header = id
    {-# INLINE header #-}

pat :: PESPSIFunc PAT
pat 0 = runPsi getF
  where
    getF h = PAT h <$> foldM (\m _ -> do
        genre <- getWord16be
        pid   <- fromIntegral . (0x1FFF .&.) <$> getWord16be
        return $ if genre == 0
                 then m
                 else IM.insert (fromIntegral genre) pid m
        ) IM.empty [1 .. ((sectionLength h - 9) `quot` 4)]

pat _ = const []

data PMT
    = PMT
        { pmtPsiHeader   :: {-#UNPACK#-}!PSIHeader
        , pmtPcrPid      :: {-#UNPACK#-}!Word16
        , pmtDescriptors :: !Descriptors
        , pmtStreams     :: !(IM.IntMap (Word8, Descriptors))
        }
        deriving Show

instance PSI PMT where
    header = header . pmtPsiHeader

pmt :: (Int -> Bool) -> PESPSIFunc PMT
pmt f i | f i       = runPsi getF
        | otherwise = const []
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
    = StreamIdDescriptor {-#UNPACK#-}!Word8
    | ShortEventDescriptor
        { sedLanguage   :: {-#UNPACK#-}!S.ByteString 
        , sedTitle      :: L.ByteString
        , sedDecription :: L.ByteString
        }
    | Raw L.ByteString
    deriving Show

getDescriptor :: Get (Int, Int, Descriptor)
getDescriptor = getWord8 >>= \case
    0x52 -> getWord8 >> ((3,0x52,) . StreamIdDescriptor <$> getWord8)
    0x4D -> do
        len  <- getWord8
        lang <- getByteString 3
        pLen <- fromIntegral <$> getWord8
        titl <- decodeUtf8 =<< getLazyByteString pLen
        dLen <- fromIntegral <$> getWord8
        (fromIntegral $ len + 2, 0x4d,) . ShortEventDescriptor lang titl <$> (decodeUtf8 =<< getLazyByteString dLen)
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

eit :: PESPSIFunc EIT
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

raw :: PESPSIFunc L.ByteString
raw _ = (:[])

type PESPSIFunc a = Int -> L.ByteString -> [a]
type WrappedFunc  = PESPSI L.ByteString -> [Wrapper]

wrap :: (Typeable a, Show a) => (PESPSIFunc a) -> WrappedFunc
wrap f p@PESPSI{..} = (\b -> Wrap $ p {pesPsiPayload = b}) <$> f pesPsiProgramId pesPsiPayload 

data WrappedFuncs = forall a. (Typeable a, Show a) => (PESPSIFunc a) :-> WrappedFuncs
                  | END 
infixr :->
infixr -|

(-|) :: (Typeable a, Typeable b, Show a, Show b) => PESPSIFunc a -> PESPSIFunc b -> WrappedFuncs
a -| b = a :-> b :-> END

multiPESPSI :: Monad m => WrappedFuncs -> Conduit (PESPSI L.ByteString) m Wrapper
multiPESPSI = awaitForever . go
  where
    go END        _ = return ()
    go (f :-> fs) r = case f (pesPsiProgramId r) (pesPsiPayload r) of
        [] -> go fs r
        m  -> mapM_ (yield . Wrap . (\p -> r{pesPsiPayload = p})) m

singlePESPSI :: Monad m => PESPSIFunc a -> Conduit (PESPSI L.ByteString) m (PESPSI a)
singlePESPSI f = awaitForever $ \r ->
    case f (pesPsiProgramId r) (pesPsiPayload r) of
        [] -> return ()
        m  -> mapM_ (\a -> yield r{pesPsiPayload = a}) m
