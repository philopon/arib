
{-# LANGUAGE TemplateHaskell #-}

module Data.Arib.PSI.Descriptor.Internal.TH where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH
import Data.Typeable
import Data.Bits
import Data.Word

import Data.Arib.PSI.Internal.Common

type Genre = (Int, String, String)
readData :: String -> [(Genre, [Genre])]
readData = go . lines
  where
    parseGenre l = let (u, o) = break (== '\t') l
                       (t, s) = break (== '\t') $ tail o
                   in (read u :: Int, t, tail s)

    go []                      = []
    go (l:ls) 
        | null l               = go ls
        | not $ head l == '\t' = let (ls', s) = feed ls in (parseGenre l, s) : go ls'
        | otherwise            = []

    feed [] = ([], [])
    feed (l:ls)
        | null l         = feed ls
        | head l == '\t' = (parseGenre (tail l) :) <$> feed ls
        | otherwise      = (l:ls, [])

mkGenre :: FilePath -> DecsQ
mkGenre file = do
    g <- runIO $ readData <$> readFile file
    subGenreD <- forM g $ \((_,gn,_), sg) -> 
        dataD (return []) (mkName gn) [] 
            (map (\(_,sgn,_) -> normalC (mkName $ gn ++ sgn) []) sg ++ 
             [ normalC (mkName $ "Other" ++ gn) []
             , normalC (mkName $ "Undefined" ++ gn) [return (Unpacked, ConT ''Word8)]
             ]) derive

    genreD <- dataD (return []) (mkName "Genre") [] 
        (map (\((_,gn,_),_) -> normalC (mkName gn) [return (NotStrict, ConT $ mkName gn)]) g ++
         [ normalC (mkName "OtherGenre") []
         , normalC (mkName "UndefinedGenre") [return (Unpacked, ConT ''Word8)]
         ]) derive

    fromBinaryD <- instanceD (return []) [t|FromBinary $(conT $ mkName "Genre")|]
         [ tySynInstD ''BinaryRep $ tySynEqn [conT $ mkName "Genre"] (conT ''Word8)
         , funD     'fromBinary [clause [varP $ mkName "w"] (normalB $ caseE [|shiftR $(varE $ mkName "w") 4|] (genreMatch g)) []]
         , pragInlD 'fromBinary Inline FunLike AllPhases
         ]

    prettyGenreD <- instanceD (return []) [t|Pretty $(conT $ mkName "Genre")|]
         [ funD 'pretty $ map (prettyGenre . fst) g ++ 
             [ clause [conP (mkName "OtherGenre") []] (normalB $ stringE "その他") []
             , clause [conP (mkName "UndefinedGenre") [varP $ mkName "w"]] (normalB [|"未定義[" ++ show $(varE $ mkName "w") ++ "]"|]) []
             ]
         , pragInlD 'pretty Inline FunLike AllPhases
         ]
    prettySubGenreD <- forM g $ \((_,gn,_), sg) ->
         instanceD (return []) [t|Pretty $(conT $ mkName gn)|]
         [ funD 'pretty $ map (prettySubGenre gn) sg ++
             [ clause [conP (mkName $ "Other" ++ gn) []] (normalB $ stringE "その他") []
             , clause [conP (mkName $ "Undefined" ++ gn) [varP $ mkName "w"]] (normalB [|"未定義[" ++ show $(varE $ mkName "w") ++ "]"|]) []
             ]
         , pragInlD 'pretty Inline FunLike AllPhases
         ]
    prettySubGenreFD <- funD (mkName "prettySubGenre") $ 
        map (prettySubGenreF . fst) g ++
        [ clause [conP (mkName "OtherGenre") []] (normalB [|"その他"|]) []
        , clause [conP (mkName "UndefinedGenre") [varP $ mkName "s"]] (normalB [|"未定義[" ++ show $(varE $ mkName "s") ++ "]"|]) []
        ]
    prettySubGenreSig <- sigD (mkName "prettySubGenre") [t| $(conT $ mkName "Genre") -> String |]
    return $ subGenreD ++ genreD : fromBinaryD : prettyGenreD : prettySubGenreD ++ prettySubGenreFD : prettySubGenreSig : []
  where
    derive = [''Show, ''Read, ''Eq, ''Ord, ''Typeable]
    genreMatch g = map ( \((gi, gn, _), sg) ->
        match (litP . integerL $ fromIntegral gi)
            (normalB [| $(conE $ mkName gn) $ $(caseE [| $(varE $ mkName "w") .&. 0xF |] $ subGenreMatch gn sg) |]) []
        ) g ++ [ match (litP $ integerL 0xF) (normalB . conE . mkName $ "OtherGenre") []
               , match wildP (normalB [|$(conE $ mkName "UndefinedGenre") $(varE $ mkName "w") |]) []
               ]
    subGenreMatch gn sg =  map (\(sgi, sgn, _) -> 
        match (litP . integerL $ fromIntegral sgi)
            (normalB . conE . mkName $ gn ++ sgn) []
        ) sg ++ [ match (litP $ integerL 0xF) (normalB . conE . mkName $ "Other" ++ gn) []
                , match wildP (normalB [|$(conE . mkName $ "Undefined" ++ gn) $(varE $ mkName "w")|]) []
                ]

    prettyGenre       (_, gn, gd) = clause [conP (mkName gn) [wildP]] (normalB $ stringE gd) []
    prettySubGenre gn (_,sgn,sgd) = clause [conP (mkName $ gn ++ sgn) []] (normalB $ stringE sgd) []
    prettySubGenreF   (_, gn, _ ) = clause [conP (mkName gn) [varP $ mkName "s"]] (normalB [|pretty $(varE $ mkName "s")|]) []
