module Distribution exposing (..)

import Utility

import Round 


-- TODO: NaNになってしまうのなんとかしたい。
{-| hypergeometric -- 超幾何分布
hypergeometric 1000 200 5 1
OUT 0.41063
-}
hypergeometric:Int -> Int -> Int -> Int -> Float
hypergeometric nN m n f =
    case f of
        0 -> 
            Utility.combinationStarling (nN - m) n / Utility.combinationStarling nN n 

        _ ->
            ( ( Utility.combinationStarling (nN - m) (n - f) * Utility.combinationStarling m f ) |> Round.roundNum 4) / ( (Utility.combinationStarling nN n) |> Round.roundNum 4 )


