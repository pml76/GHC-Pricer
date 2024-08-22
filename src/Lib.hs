module Lib
    ( someFunc
    , calcPrice

    ) where

import qualified Numeric.Integration.TanhSinh as NI

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype InterestRate = InterestRate Double
newtype Price = Price Double
newtype Volatility = Volatility Double

priceToDouble :: Price -> Double 
priceToDouble (Price d) = d 

interestRateToDouble :: InterestRate -> Double
interestRateToDouble (InterestRate d) = d

volatilityToDouble :: Volatility -> Double
volatilityToDouble (Volatility d) = d 




class Market a where
    interestRate :: a -> InterestRate
    underlying   :: a -> Price
    timeToMaturity :: a -> Double
    volatility :: a -> Volatility

calcPrice :: Market m => m -> (Double -> Double) -> Double
calcPrice m payout = 
    let r           = (interestRateToDouble . interestRate) m
        x           = (priceToDouble . underlying) m
        tau         = timeToMaturity m
        sigma       = (volatilityToDouble . volatility) m
        f y         = x * exp (tau * r - sigma * sqrt tau * y ) 
        r2           = NI.nonNegative NI.trap f
        (low, high) = (NI.confidence . NI.absolute 6.0e-10) r2
    in (low + high) / 2
             
