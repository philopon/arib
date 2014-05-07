{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
import Data.Conduit
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import System.Environment
import Data.Maybe
import Numeric

import Data.Arib

printPMT :: Int -> Int -> PMT -> IO ()
printPMT program pid p = putStrLn . unlines $
    (show program ++ "(pid:" ++ show pid ++ ")") :
    map (\s ->
        let d = streamDescriptors s
        in "  elementalyPID=0x" ++ showHex (elementalyPID s)
           ", type=" ++ show (streamType s) ++
           (maybe "" (\(ComponentTag i) -> ", streamId=" ++ show i) . listToMaybe $ streamId d) ++
           (maybe "" (\v -> ", resolution=" ++ (pretty . videoEncodeFormat) v) . listToMaybe $ videoDecodeControl d)
    ) (pmtStreams p)

main :: IO ()
main = getArgs >>= \case
    [file] -> runResourceT $ sourceTs file $= concatTsPackets_ $$ do
        Just h <- singlePSI pat =$ CL.peek
        forM_ (programPidMap h) $ \(program, pid) -> do
            Just p <- singlePSI (pmt pid) =$ CL.peek
            liftIO $ printPMT program pid p
    _ -> getProgName >>= \p -> putStrLn $ "USAGE: " ++ p ++ " TS"
