{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Data.Arib.String.Internal.TH where

import Numeric
import Language.Haskell.TH
import Control.Applicative
import qualified Data.IntMap.Strict as IM
import qualified Data.Primitive.Array as A
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Lazy.Builder as B

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy sep list = case break sep list of
    (bf, []) -> [bf]
    (bf, af) -> bf : splitBy sep (tail af)

readCharset :: String -> IM.IntMap String
readCharset = IM.fromList . map readDatum . lines
  where
    readDatum str = let k:v = map readField $ splitBy (== '\t') str
                    in (k, map toEnum v)
    readField s = case readHex s of
        [(v,[])] -> v
        _        -> error "Data.Arib.String.TH.readData: cannot read."

mkCharsetFunction :: FilePath -> ExpQ
mkCharsetFunction file = do
    dict   <- runIO $ readCharset <$> readFile file
    let writes = mapMaybe (\(sec,point) -> case IM.lookup (256 * (sec + 33) + (point + 33)) dict of
            Nothing -> Nothing
            Just v  -> Just [|A.writeArray $(varE $ mkName "array")
                                           $(litE . IntegerL $ fromIntegral $ 94 * sec + point) 
                                           (B.fromString $(litE $ stringL v))
                             |]) [(sec, point) | sec <- [0..93], point <- [0..93]]
    doE $
        bindS (varP $ mkName "array") [|A.newArray 8836 mempty|] :
        map noBindS writes ++
        [noBindS [|A.unsafeFreezeArray $(varE $ mkName "array")|]]

