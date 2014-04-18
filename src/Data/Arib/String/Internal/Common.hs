{-# LANGUAGE CPP #-}
module Data.Arib.String.Internal.Common where

#ifdef DEBUG
import qualified Debug.Trace
debug = Debug.Trace.trace
#else
debug _ = id
#endif
debug :: String -> a -> a


