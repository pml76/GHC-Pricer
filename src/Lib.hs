{-# LANGUAGE BangPatterns #-}

module Lib
    ( someFunc
    , OptionMarket(..)
    , EuropeanOptionDateInfo(..)
    , AsianOptionDateInfo(..)
    , Act360AsianOptionDateData(..)

    ) where

import qualified Numeric.Integration.TanhSinh as NI
import Data.Time

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


class EuropeanOptionDateInfo d where
    tradeDate :: d -> Day
    maturity :: d -> Day
    maturityPayDate  :: d -> Day
    premiumPayDate :: d -> Day

    yearsToMaturity :: d -> Double
    yearsToMaturityPayDate :: d -> Double
    yearsToPremiumPayDate :: d -> Day

class EuropeanOptionDateInfo d => AsianOptionDateInfo d where
    beginPeriod :: d -> Day
    endPeriod :: d -> Day
    endPeriod = maturity

    yearsToPeriodBegin :: d -> Double
    yearsToPeriodBegin = yearsToMaturity

    yearsToPeriodEnd :: d -> Double
    yearsInPeriod :: d -> Double

data Act360AsianOptionDateData = Act360AsianOptionDateData {
      tradeDate_ :: Day
    , maturity_ :: Day
    , beginPeriod_ :: Day
    , maturityPayDate_ :: Day 
    , premiumPayDate_ :: Day
}


act360TimeDifferenceInYears :: Day -> Day -> Double 
act360TimeDifferenceInYears d1 d2 =

    
instance EuropeanOptionDateInfo Act360AsianOptionDateData where

    tradeDate = tradeDate_
    maturity = maturity_
    maturityPayDate = maturityPayDate_
    premiumPayDate = premiumPayDate_

    yearsToMaturity d = act360TimeDifferenceInYears tradeDate_ maturity_


class OptionMarket a where
    interestRate :: EuropeanOptionDateInfo d => a -> d -> InterestRate
    underlying   :: EuropeanOptionDateInfo d => a -> d -> Price
    volatility   :: EuropeanOptionDateInfo d => a -> d -> Volatility

    europeanOptionPrice :: EuropeanOptionDateInfo d => a -> (Double -> Double) -> d -> Double
    europeanOptionPrice m payout d =
        let !r           = (interestRateToDouble . interestRate m) d
            !x           = (priceToDouble . underlying m)  d
            !sigma       = (volatilityToDouble . volatility m) d
            !tau         = yearsToMaturity d
            f y          = x * exp ( - (tau * r) ) * (payout . exp) ( - (sigma * sqrt tau * y) + (r-sigma*sigma/2)*tau) * exp (- (y * y / 2))
            !result      = NI.everywhere NI.trap f
            (!low, !high) = (NI.confidence . NI.absolute 6.0e-10) result
        in (low + high) / 2

