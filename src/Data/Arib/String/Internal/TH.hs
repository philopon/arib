{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Data.Arib.String.Internal.TH where

import Numeric
import Language.Haskell.TH
import Control.Applicative
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy sep list = case break sep list of
    (bf, []) -> [bf]
    (bf, af) -> bf : splitBy sep (tail af)

readCharset :: String -> IM.IntMap T.Text
readCharset = IM.fromList . map readDatum . lines
  where
    readDatum str = let k:v = map readField $ splitBy (== '\t') str
                        in (k, T.pack $ map toEnum v)
    readField s = case readHex s of
        [(v,[])] -> v
        _        -> error "Data.Arib.String.TH.readData: cannot read."

data Dict = 
    Dict { elemLength :: {-#UNPACK#-}!Int
         , body       :: {-#UNPACK#-}!S.ByteString
         } deriving Show

mkCharsetFunction :: FilePath -> ExpQ
mkCharsetFunction file = do
    dict <- runIO $ IM.map T.encodeUtf8 . readCharset <$> readFile file
    let maxLen = maximum . map (S.length . snd) $ IM.toList dict
        bdy    = foldl (\acc (sec,point) -> case IM.lookup (0x100 * (sec + 0x21) + (point + 0x21)) dict of
            Nothing -> acc `L.append` pad (maxLen + 1)
            Just t  -> let l = S.length t
                       in acc `L.append` (fromIntegral l `L.cons` L.fromStrict t `L.append` pad (maxLen - l))
            ) L.empty [(sec, point) | sec <- [0..93], point <- [0..93]]
    [|Dict $(litE . integerL . fromIntegral $ maxLen + 1) $(stringE $ LC.unpack bdy) |]
  where pad l = L.replicate (fromIntegral l) 0

