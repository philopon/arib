{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Arib.String.Internal.TH where

import Numeric
import Language.Haskell.TH
import Control.Applicative
import Control.Arrow(first)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.Vector.Storable as VS
import Data.Text.Encoding 
import Data.List
import Foreign.Storable
import Data.Bits
import Data.Word

import Data.Text.Internal.Unsafe(inlinePerformIO)
import Foreign.ForeignPtr
import GHC.Ptr (Ptr(..))

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn sep list = case break sep list of
    (bf, []) -> [bf]
    (bf, af) -> bf : splitOn sep (tail af)

readCharset :: String -> IM.IntMap T.Text
readCharset = IM.fromList . map readDatum . lines
  where
    readDatum str = let k:v = map readField $ splitOn (== '\t') str
                        in (k, T.pack $ map toEnum v)
    readField s = case readHex s of
        [(v,[])] -> v
        _        -> error "Data.Arib.String.TH.readData: cannot read."

data Dict = 
    Dict { elemLength :: {-#UNPACK#-}!Int
         , body       :: {-#UNPACK#-}!(VS.Vector Word16)
         } deriving Show

newtype Simple = Simple Dict deriving Show

toWord16s :: T.Text -> [Word16]
toWord16s t = unfoldr (\b -> case first S.unpack $ S.splitAt 2 b of
    ([x,y], b') -> Just (shiftL (fromIntegral x) 8 .|. fromIntegral y :: Word16, b')
    _           -> Nothing
    ) $ encodeUtf16BE t

charset' :: [Int] -> FilePath -> IO (Bool, Int, [Word16])
charset' range file = do
    dict <- IM.map toWord16s . readCharset <$> readFile file
    let maxLen = maximum . map (length . snd) $ IM.toList dict
        minLen = minimum . map (length . snd) $ IM.toList dict
        simple = maxLen == minLen
        eLen   = if simple then maxLen else maxLen + 1
        elems  = foldr (\point b -> case IM.lookup point dict of
            Nothing -> replicate eLen 0 ++ b
            Just t  -> take      eLen ((if simple then t else fromIntegral (length t) : t) ++ repeat 0) ++ b
            ) [] range
    return (simple, eLen, elems)

charset1 :: FilePath -> IO (Bool, Int, [Word16])
charset1 = charset' [0x21 .. 0x7E]

charset2 :: FilePath -> IO (Bool, Int, [Word16])
charset2 = charset' [sec * 0x100 + point | sec <- [0x21 .. 0x7E], point <- [0x21 .. 0x7E]]

toStorableVectorIR :: forall a. Storable a => [a] -> IO [Word8]
toStorableVectorIR l = VS.unsafeWith (VS.fromList l) $ \p ->
    mapM (peekByteOff p) [ 0 .. length l * sizeOf (undefined :: a) - 1]

mkCharset :: (FilePath -> IO (Bool, Int, [Word16])) -> FilePath -> Q Exp
mkCharset charset file = do
    (simple, maxLen, v) <- runIO $ charset file
    ir                  <- runIO $ toStorableVectorIR v
    [|$(if simple then [|Simple|] else [|id|]) $ Dict
        $(litE . integerL $ fromIntegral maxLen)
        (VS.unsafeFromForeignPtr0
            (inlinePerformIO . newForeignPtr_ $ Ptr $(litE $ StringPrimL ir)) 
            $(litE . integerL . fromIntegral $ length v)) |]
