{-# LANGUAGE BangPatterns #-}

module Lib
    ( someFunc
    , OptionMarket(..)

    ) where

import qualified Numeric.Integration.TanhSinh as NI
import qualified Data.Time.Clock as TC

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




class OptionMarket a where
    interestRate :: a -> TC.DiffTime -> InterestRate
    underlying   :: a -> TC.DiffTime -> Price
    volatility :: a -> TC.DiffTime -> Volatility

    europeanOptionPrice :: a -> (Double -> Double) -> TC.DiffTime -> Double
    europeanOptionPrice m payout t =
        let !r           = (interestRateToDouble . interestRate m) t
            !x           = (priceToDouble . underlying m)  t
            !sigma       = (volatilityToDouble . volatility m) t
            !tau         = (fromIntegral . toInteger) t / (360 * 24 * 3600)
            f y         = x * exp ( - (tau * r) ) * (payout . exp) ( - (sigma * sqrt tau * y) + (r-sigma*sigma/2)*tau) * exp (- (y * y / 2))
            !result      = NI.everywhere NI.trap f
            (!low, !high) = (NI.confidence . NI.absolute 6.0e-10) result
        in (low + high) / 2

