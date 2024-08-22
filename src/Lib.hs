module Lib
    ( someFunc
    , calcPrice

    ) where

import qualified Numeric.Integration.TanhSinh as NI
import qualified Statistics.Distribution as SD

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype InterestRate = InterestRate Double
newtype Price = Price Double

class Market a where
    interestRate :: a -> InterestRate
    underlying   :: a -> Price
    logDensity :: a -> Double -> Double
    timeToMaturity :: a -> Double
    volatility :: a -> Double

calcPrice :: Market m => m -> (Double -> Double) -> Double
calcPrice m payout = 
    let f y         = underlying m * exp (timeToMaturity m * interestRate m - volatility m * sqrt (timeToMaturity m) * y) 
        r           = NI.nonNegative NI.trap f
        (low, high) = (NI.confidence . NI.absolute 6.0e-10) r
    in (low + high) / 2
             
