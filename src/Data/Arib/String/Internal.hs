{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Data.Arib.String.Internal where

import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Error

import Numeric
import Data.Bits
import Data.Word
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B
import qualified Data.Map.Strict as M

import Data.Conduit
import qualified Data.Conduit.Binary as CB

import Data.Arib.String.Internal.Types
import Data.Arib.String.Internal.Common
import Data.Arib.String.Internal.Charset

finToGSet1 :: AribConfig a -> Word8 -> Either Word8 (GetChar a)
finToGSet1 c 0x4A = debug "eisuu <-" Right (GetChar1 $ eisuu c)
finToGSet1 c 0x30 = debug "hiragana <-" Right (GetChar1 $ hiragana c)
finToGSet1 c 0x31 = debug "katakana <-" Right (GetChar1 $ katakana c)
finToGSet1 c 0x32 = debug "mosaicA <-" Right (GetChar1 $ mosaicA c)
finToGSet1 c 0x33 = debug "mosaicB <-" Right (GetChar1 $ mosaicB c)
finToGSet1 c 0x34 = debug "mosaicC <-" Right (GetChar1 $ mosaicC c)
finToGSet1 c 0x35 = debug "mosaicD <-" Right (GetChar1 $ mosaicD c)
finToGSet1 c 0x36 = debug "katakanaP <-" Right (GetChar1 $ katakanaP c)
finToGSet1 c 0x37 = debug "hiraganaP <-" Right (GetChar1 $ hiraganaP c)
finToGSet1 c 0x38 = debug "katakanaP <-" Right (GetChar1 $ katakanaP c)
finToGSet1 c 0x49 = debug "jisKatakana <-" Right (GetChar1 $ jisKatakana c)
finToGSet1 _ w    = Left w

finToGSet2 :: AribConfig a -> Word8 -> Either Word8 (GetChar a)
finToGSet2 c 0x42 = debug "kanji <-"      Right (GetChar2 $ kanji c)
finToGSet2 c 0x39 = debug "jisKanji1 <-"  Right (GetChar2 $ jisKanji1 c)
finToGSet2 c 0x3A = debug "jisKanji2 <-"  Right (GetChar2 $ jisKanji2 c)
finToGSet2 c 0x3B = debug "additional <-" Right (GetChar2 $ additional c)
finToGSet2 _ w    = Left w

finToDRCS2 :: AribConfig a -> Word8 -> Either Word8 (GetChar a)
finToDRCS2 c 0x40 = debug "DRCS0 <-" Right (GetChar2 $ drcs0 c)
finToDRCS2 _ w    = Left w

finToDRCS1 :: AribConfig a -> Word8 -> Either Word8 (GetChar a)
finToDRCS1 c 0x41 = debug "DRCS1 <-"  Right (GetChar1 $ drcs1  c)
finToDRCS1 c 0x42 = debug "DRCS2 <-"  Right (GetChar1 $ drcs2  c)
finToDRCS1 c 0x43 = debug "DRCS3 <-"  Right (GetChar1 $ drcs3  c)
finToDRCS1 c 0x44 = debug "DRCS4 <-"  Right (GetChar1 $ drcs4  c)
finToDRCS1 c 0x45 = debug "DRCS5 <-"  Right (GetChar1 $ drcs5  c)
finToDRCS1 c 0x46 = debug "DRCS6 <-"  Right (GetChar1 $ drcs6  c)
finToDRCS1 c 0x47 = debug "DRCS7 <-"  Right (GetChar1 $ drcs7  c)
finToDRCS1 c 0x48 = debug "DRCS8 <-"  Right (GetChar1 $ drcs8  c)
finToDRCS1 c 0x49 = debug "DRCS9 <-"  Right (GetChar1 $ drcs9  c)
finToDRCS1 c 0x4A = debug "DRCS10 <-" Right (GetChar1 $ drcs10 c)
finToDRCS1 c 0x4B = debug "DRCS11 <-" Right (GetChar1 $ drcs11 c)
finToDRCS1 c 0x4C = debug "DRCS12 <-" Right (GetChar1 $ drcs12 c)
finToDRCS1 c 0x4D = debug "DRCS13 <-" Right (GetChar1 $ drcs13 c)
finToDRCS1 c 0x4E = debug "DRCS14 <-" Right (GetChar1 $ drcs14 c)
finToDRCS1 c 0x4F = debug "DRCS15 <-" Right (GetChar1 $ drcs15 c)
finToDRCS1 _ 0x70 = debug "MACRO <-" Right Macro
finToDRCS1 _ w    = Left w

await_ :: MonadError AribStringException m => Consumer S.ByteString m Word8
await_ = CB.head >>= \case
    Nothing -> throwError IllegalEndOfInput
    Just a  -> return a

awaitGSet1, awaitGSet2, awaitDRCS1, awaitDRCS2
    :: (MonadReader (AribConfig a) m, MonadError AribStringException m) => Consumer S.ByteString m (Either Word8 (GetChar a))
awaitGSet1 = finToGSet1 <$> ask <*> await_
awaitGSet2 = finToGSet2 <$> ask <*> await_

awaitDRCS1 = finToDRCS1 <$> ask <*> await_
awaitDRCS2 = finToDRCS2 <$> ask <*> await_

data AribStringException
    = UnknownEscapeSequence [Word8]
    | IllegalEndOfInput
    | WantInput String
    deriving Show

processEscape' :: (MonadError AribStringException m, MonadState s m) 
               => [Word8] -> m (Either Word8 a) -> (a -> s -> s) -> Word8 -> m ()
processEscape' ws awit field 0x20 = awit >>=
    either (\w -> throwError . UnknownEscapeSequence $ ws ++ [0x20,w]) (modify . field)
processEscape' ws _    _     w    = throwError . UnknownEscapeSequence $ ws ++ [w]

processEscape :: (MonadState (AribState a) m, MonadReader (AribConfig a) m, MonadError AribStringException m)
              => Word8 -> Consumer S.ByteString m ()
processEscape 0x6E = debug "LS2"  modify $ glTo g2 -- LS2
processEscape 0x6F = debug "LS3"  modify $ glTo g3 -- LS3
processEscape 0x7E = debug "LS1R" modify $ grTo g1 -- LS1R
processEscape 0x7D = debug "LS2R" modify $ grTo g2 -- LS2R
processEscape 0x7C = debug "LS3R" modify $ grTo g3 -- LS3R

processEscape 0x28 = awaitGSet1 >>= either (processEscape' [0x28] awaitDRCS1 g0To) (debug "G0 ->" modify . g0To)
processEscape 0x29 = awaitGSet1 >>= either (processEscape' [0x29] awaitDRCS1 g1To) (debug "G1 ->" modify . g1To)
processEscape 0x2A = awaitGSet1 >>= either (processEscape' [0x2A] awaitDRCS1 g2To) (debug "G2 ->" modify . g2To)
processEscape 0x2B = awaitGSet1 >>= either (processEscape' [0x2B] awaitDRCS1 g3To) (debug "G3 ->" modify . g3To)

processEscape 0x24 = awaitGSet2 >>= either notG0Process (debug "G0 ->" modify . g0To)
  where
    notG0Process 0x28 = await_     >>=         processEscape' [0x24,0x28] awaitDRCS2 g0To
    notG0Process 0x29 = awaitGSet2 >>= either (processEscape' [0x24,0x29] awaitDRCS2 g1To) (debug "G1 -> " modify . g1To)
    notG0Process 0x2A = awaitGSet2 >>= either (processEscape' [0x24,0x2A] awaitDRCS2 g2To) (debug "G2 -> " modify . g2To)
    notG0Process 0x2B = awaitGSet2 >>= either (processEscape' [0x24,0x2B] awaitDRCS2 g3To) (debug "G3 -> " modify . g3To)
    notG0Process w    = throwError $ UnknownEscapeSequence [0x24,w]

processEscape w = throwError $ UnknownEscapeSequence [w]

applyGetChar :: ( MonadState (AribState a) m, MonadError AribStringException m
                , MonadReader (AribConfig a) m, MonadWriter a m)
             => (AribState a -> GetChar a) -> Word8 -> Consumer S.ByteString m ()
applyGetChar ptr w = gets ptr >>= \case
    GetChar1 f -> tell (f $ clearBit w 7)
    GetChar2 f -> await_ >>= \x -> tell $ f (clearBit w 7) (clearBit x 7)
    Macro      -> do
        macro <- maybe L.empty id . M.lookup (clearBit w 7) <$> gets macros
        debug ("Macro" ++ showHex (clearBit w 7) (' ': show macro)) CB.sourceLbs macro $$ process

localState :: MonadState s m => (s -> s) -> m a -> m a
localState f m = get >>= \st -> put (f st) >> m >>= \r -> put st >> return r
{-# INLINE localState #-}

-- TODO: Macro, CSI
processC :: ( MonadState (AribState a) m, MonadReader (AribConfig a) m
            , MonadError AribStringException m, MonadWriter a m) 
         => Word8 -> Consumer S.ByteString m ()
processC 0x0F = modify (glTo g0) >> process         -- LS0
processC 0x0E = modify (glTo g1) >> process         -- LS1
processC 0x1B = await_ >>= processEscape >> process -- ESC
processC 0x19 = localState (glTo g2) process        -- SS2
processC 0x1D = localState (glTo g3) process        -- SS3
processC w
    | w `elem` just1  = asks control <*> pure Control <*> pure w <*> ((:[]) <$> await_) >>= tell
    | w `elem` just2  = asks control <*> pure Control <*> pure w <*> (L.unpack <$> CB.take 2) >>= tell
    | w `elem` oneOr2 = await_ >>= \case
        0x20 -> asks control <*> pure Control <*> pure w <*> ((:[]) <$> await_) >>= tell
        c    -> asks control <*> pure Control <*> pure w <*> pure [c] >>= tell
    | otherwise       = asks control <*> pure Control <*> pure w <*> pure [] >>= tell
  where
    oneOr2 = [0x90,0x92]
    just1  = [0x16,0x8B,0x91,0x93,0x94,0x97,0x98]
    just2  = [0x1C,0x9D] -- APS TIME

process' :: ( MonadError AribStringException m, MonadReader (AribConfig a) m
            , MonadState (AribState a) m, MonadWriter a m) => Word8 -> Consumer S.ByteString m ()
process' 0xFF = processC 0xFF
process' w |              w < 0x21 = processC w
           | 0x21 <= w && w < 0x7F = applyGetChar gl w
           | 0x7F <= w && w < 0xA1 = processC w
           | otherwise             = applyGetChar gr w

process :: ( MonadState (AribState a) m, MonadReader (AribConfig a) m
           , MonadError AribStringException m, MonadWriter a m) => Consumer S.ByteString m ()
process = CB.head >>= \case
    Nothing -> return ()
    Just a  -> process' a >> process

-- | decode arib string to utf8 encoded bytestring.  
--   when decode error, call fail method of Monad instance.
decodeUtf8 :: Monad m => L.ByteString -> m L.ByteString
decodeUtf8 str = 
    either (fail . show) return . fmap (B.toLazyByteString . snd) $
    evalRWST (CB.sourceLbs str $$ process) utf8Config (initialState utf8Config)
