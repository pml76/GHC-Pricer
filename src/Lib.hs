{-# LANGUAGE BangPatterns #-}

module Lib
    ( someFunc
    , Market(..)

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
    volatility :: a -> Volatility

    optionPrice :: a -> (Double -> Double) -> Double -> Double
    optionPrice m payout tau =
        let !r           = (interestRateToDouble . interestRate) m
            !x           = (priceToDouble . underlying) m
            !sigma       = (volatilityToDouble . volatility) m
            f y         = x * exp ( - (tau * r) ) * (payout . exp) ( - (sigma * sqrt tau * y) + (r-sigma*sigma/2)*tau) * exp (- (y * y / 2))
            !result      = NI.everywhere NI.trap f
            (!low, !high) = (NI.confidence . NI.absolute 6.0e-10) result
        in (low + high) / 2

