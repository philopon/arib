{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Data.Arib.String.Internal where

import Pipes
import Control.Monad.RWS.Strict
import Control.Monad.Error

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as L

import Data.Arib.String.Internal.Pipes
import Data.Arib.String.Internal.Debug
import Data.Arib.String.Internal.Types
import Data.Arib.String.Internal.Common

finToGSet1 :: Word8 -> Either Word8 (AribConfig a -> GSet1 a)
finToGSet1 0x4A = debug "eisuu <-" Right eisuu
finToGSet1 0x30 = debug "hiragana <-" Right hiragana
finToGSet1 0x31 = debug "katakana <-" Right katakana
finToGSet1 0x32 = debug "mosaicA <-" Right mosaicA
finToGSet1 0x33 = debug "mosaicB <-" Right mosaicB
finToGSet1 0x34 = debug "mosaicC <-" Right mosaicC
finToGSet1 0x35 = debug "mosaicD <-" Right mosaicD
finToGSet1 0x36 = debug "katakanaP <-" Right katakanaP
finToGSet1 0x37 = debug "hiraganaP <-" Right hiraganaP
finToGSet1 0x38 = debug "katakanaP <-" Right katakanaP
finToGSet1 0x49 = debug "jisKatakana <-" Right jisKatakana
finToGSet1 w    = Left w

finToGSet2 :: Word8 -> Either Word8 (AribConfig a -> GSet2 a)
finToGSet2 0x42 = debug "kanji <-"      Right kanji
finToGSet2 0x39 = debug "jisKanji1 <-"  Right jisKanji1
finToGSet2 0x3A = debug "jisKanji2 <-"  Right jisKanji2
finToGSet2 0x3B = debug "additional <-" Right additional
finToGSet2 w    = Left w

finToDRCS2 :: Word8 -> Either Word8 (AribConfig a -> DRCS2 a)
finToDRCS2 0x40 = debug "DRCS0 <-" Right drcs0
finToDRCS2 w    = Left w

finToDRCS1 :: Word8 -> Either Word8 (AribConfig a -> DRCS1 a)
finToDRCS1 0x41 = debug "DRCS1 <-" Right drcs1
finToDRCS1 0x42 = debug "DRCS2 <-" Right drcs2
finToDRCS1 0x43 = debug "DRCS3 <-" Right drcs3
finToDRCS1 0x44 = debug "DRCS4 <-" Right drcs4
finToDRCS1 0x45 = debug "DRCS5 <-" Right drcs5
finToDRCS1 0x46 = debug "DRCS6 <-" Right drcs6
finToDRCS1 0x47 = debug "DRCS7 <-" Right drcs7
finToDRCS1 0x48 = debug "DRCS8 <-" Right drcs8
finToDRCS1 0x49 = debug "DRCS9 <-" Right drcs9
finToDRCS1 0x4A = debug "DRCS10 <-" Right drcs10
finToDRCS1 0x4B = debug "DRCS11 <-" Right drcs11
finToDRCS1 0x4C = debug "DRCS12 <-" Right drcs12
finToDRCS1 0x4D = debug "DRCS13 <-" Right drcs13
finToDRCS1 0x4E = debug "DRCS14 <-" Right drcs14
finToDRCS1 0x4F = debug "DRCS15 <-" Right drcs15
finToDRCS1 w    = Left w

awaitGSet1, awaitGSet2, awaitDRCS1, awaitDRCS2
    :: MonadReader (AribConfig a) m => Consumer' Word8 m (Either Word8 (GetChar a))
awaitGSet1 = fmap finToGSet1 await >>= either (return . Left) (fmap (Right . GetChar1) . asks)
awaitGSet2 = fmap finToGSet2 await >>= either (return . Left) (fmap (Right . GetChar2) . asks)

awaitDRCS1 = fmap finToDRCS1 await >>= either (return . Left) (fmap (Right . GetChar1) . asks)
awaitDRCS2 = fmap finToDRCS2 await >>= either (return . Left) (fmap (Right . GetChar2) . asks)

data AribException
    = UnknownEscapeSequence [Word8]
    deriving Show

processEscape' :: (MonadError AribException m, MonadState s m) 
               => [Word8] -> m (Either Word8 a) -> (a -> s -> s) -> Word8 -> m ()
processEscape' ws awit field 0x20 = awit >>=
    either (\w -> throwError . UnknownEscapeSequence $ ws ++ [0x20,w]) (modify . field)
processEscape' ws _    _     w    = throwError . UnknownEscapeSequence $ ws ++ [w]

processEscape :: (MonadState (AribState a) m, MonadReader (AribConfig a) m, MonadError AribException m)
              => Word8 -> Consumer' Word8 m ()
processEscape 0x6E = debug "LS2"  modify $ glTo g2 -- LS2
processEscape 0x6F = debug "LS3"  modify $ glTo g3 -- LS3
processEscape 0x7E = debug "LS1R" modify $ grTo g1 -- LS1R
processEscape 0x7D = debug "LS2R" modify $ grTo g2 -- LS2R
processEscape 0x7C = debug "LS3R" modify $ grTo g3 -- LS3R

processEscape 0x28 = awaitGSet1 >>= either (processEscape' [0x28] awaitDRCS1 g0To) (modify . g0To)
processEscape 0x29 = awaitGSet1 >>= either (processEscape' [0x29] awaitDRCS1 g1To) (modify . g1To)
processEscape 0x2A = awaitGSet1 >>= either (processEscape' [0x2A] awaitDRCS1 g2To) (modify . g2To)
processEscape 0x2B = awaitGSet1 >>= either (processEscape' [0x2B] awaitDRCS1 g3To) (modify . g3To)

processEscape 0x24 = awaitGSet2 >>= either notG0Process (modify . g0To)
  where
    notG0Process 0x28 = await      >>= processEscape' [0x24,0x28] awaitDRCS2 g0To
    notG0Process 0x29 = awaitGSet2 >>= either (processEscape' [0x24,0x29] awaitDRCS2 g1To) (modify . g1To)
    notG0Process 0x2A = awaitGSet2 >>= either (processEscape' [0x24,0x2A] awaitDRCS2 g2To) (modify . g2To)
    notG0Process 0x2B = awaitGSet2 >>= either (processEscape' [0x24,0x2B] awaitDRCS2 g3To) (modify . g3To)
    notG0Process w    = throwError $ UnknownEscapeSequence [0x24,w]

processEscape w = throwError $ UnknownEscapeSequence [w]

applyGetChar :: MonadState (AribState a) m => (AribState a -> GetChar a) -> Word8 -> Consumer' Word8 m a
applyGetChar ptr w = gets ptr >>= \case
    GetChar1 f -> return $ f (clearBit w 7)
    GetChar2 f -> await >>= \x -> return $ f (clearBit w 7) (clearBit x 7)

localState :: MonadState s m => (s -> s) -> m a -> m a
localState f m = get >>= \st -> put (f st) >> m >>= \r -> put st >> return r

process :: (MonadError AribException m, MonadReader (AribConfig a) m, MonadState (AribState a) m)
        => Consumer' Word8 m a
process = await >>= \case
    0x0F -> modify (glTo g0) >> process
    0x0E -> modify (glTo g1) >> process
    0x1B -> await >>= processEscape >> process
    0x19 -> localState (glTo g2) process
    0x1D -> localState (glTo g3) process
    w |              w < 0x20 -> undefined -- TODO: c0
      | 0x20 <= w && w < 0x80 -> applyGetChar gl w
      | 0x80 <= w && w < 0xA0 -> undefined -- TODO: c1
      | 0xA0 <= w             -> applyGetChar gr w
    _ -> error "process: internal error."

-- http://www35.atwiki.jp/tvrock/m/pages/26.html
test :: L.ByteString
test = L.pack [0x1B, 0x7C, 0xC9, 0xE9, 0xDE, 0xA2, 0xF3, 0xB3, 0x21, 0x3C, 0xEB, 0x21, 0x56, 0x3F, 0x65, 0x38, 0x4D, 0x32, 0x2B, 0x4C, 0x67, 0x21, 0x21, 0x42, 0x68, 0x1B, 0x7E, 0xB7, 0x49, 0x74, 0x21, 0x57, 0x1B, 0x24, 0x2A, 0x3B, 0x1B, 0x7D, 0xFA, 0xEA] 

doTest :: Either AribException [DebugChar]
doTest = fmap snd $ evalRWST (runEffect ((fromLazy test) >-> forever (process >>= tell . (:[])))) debugConfig 
    ((initialState debugConfig) { g3 = GetChar1 $ katakana debugConfig})
