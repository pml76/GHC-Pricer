module Lib
    ( someFunc
    , calcPrice

    ) where

import qualified Numeric.Integration.TanhSinh as NI
import qualified Statistics.Distribution as SD

someFunc :: IO ()
someFunc = putStrLn "someFunc"

calcPrice :: (Double -> Double) -> Double
calcPrice payout = 
    let f x         = (payout x) * SD.normal  
        r           = NI.nonNegative NI.trap f
        (low, high) = (NI.confidence . NI.absolute 6.0e-10) r
    in (low + high) / 2
             
