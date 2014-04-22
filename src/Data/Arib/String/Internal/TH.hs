{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Data.Arib.String.Internal.TH where

import Numeric
import Language.Haskell.TH
import Control.Applicative
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.ByteString as S

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

mkCharset1 :: (T.Text -> String) -> FilePath -> ExpQ
mkCharset1 conv file = do
    dict <- runIO $ IM.map conv . readCharset <$> readFile file
    let maxLen = maximum . map (length . snd) $ IM.toList dict
        bdy    = foldr (\point b -> case IM.lookup point dict of
            Nothing -> pad (maxLen + 1) ++ b
            Just t  -> let l = length t
                       in (toEnum l : t ++ pad (maxLen - l)) ++ b
            ) [] [0x21 .. 0x7E]
    [|Dict $(litE . integerL . fromIntegral $ maxLen + 1) $(stringE bdy) |]
  where pad l = replicate (fromIntegral l) '\0'

mkCharset2 :: (T.Text -> String) -> FilePath -> ExpQ
mkCharset2 conv file = do
    dict <- runIO $ IM.map conv . readCharset <$> readFile file
    let maxLen = maximum . map (length . snd) $ IM.toList dict
        bdy    = foldr (\(sec,point) b -> case IM.lookup (0x100 * sec + point) dict of
            Nothing -> pad (maxLen + 1) ++ b
            Just t  -> let l = length t
                       in (toEnum l : t ++ pad (maxLen - l)) ++ b
            ) [] [(sec, point) | sec <- [0x21 .. 0x7E], point <- [0x21 .. 0x7E]]
    [|Dict $(litE . integerL . fromIntegral $ maxLen + 1) $(stringE bdy) |]
  where pad l = replicate (fromIntegral l) '\0'

