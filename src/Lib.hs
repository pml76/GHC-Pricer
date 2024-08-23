{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

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
    yearsToPremiumPayDate :: d -> Double

class EuropeanOptionDateInfo d => AsianOptionDateInfo d where

    -- |The begin of the maturity-period of the option (including)
    beginPeriod :: d -> Day

    -- |The end of the maturity period of the option (excluding)
    endPeriod :: d -> Day
    endPeriod = maturity

    -- |The length of the time window between 'tradeDate d' (including) and the 'beginPeriod d' (excluding) in years
    yearsToBeginPeriod :: d -> Double


    -- |The length of the time window between 'tradeDate d' (including) and 'endPeriod d' (excluding) in years
    yearsToEndPeriod :: d -> Double
    yearsToEndPeriod = yearsToMaturity

    -- |The length of the time window between 'beginPeriod d' (including) and 'endPeriod d' (excluding) in years 
    yearsInPeriod :: d -> Double

data Act360AsianOptionDateData = Act360AsianOptionDateData {
      tradeDate_ :: Day
    , maturity_ :: Day
    , beginPeriod_ :: Day
    , maturityPayDate_ :: Day
    , premiumPayDate_ :: Day
}


-- |Compute the time difference between d1 (including) and d2 (excluding) in years according to the Act/360 convention. 
-- When 'd1' lies before 'd2' the result is positive. 
act360TimeDifferenceInYears :: Day -> Day -> Double
act360TimeDifferenceInYears d1 d2 =
    fromInteger (toModifiedJulianDay d2 - toModifiedJulianDay d1) / 360


instance EuropeanOptionDateInfo Act360AsianOptionDateData where
    tradeDate = tradeDate_
    maturity = maturity_
    maturityPayDate = maturityPayDate_
    premiumPayDate = premiumPayDate_

    yearsToMaturity d = act360TimeDifferenceInYears (tradeDate d) (maturity d)
    yearsToMaturityPayDate d = act360TimeDifferenceInYears (tradeDate d) (maturityPayDate d)
    yearsToPremiumPayDate d = act360TimeDifferenceInYears (tradeDate d) (premiumPayDate d)

instance AsianOptionDateInfo Act360AsianOptionDateData where
    beginPeriod = beginPeriod_

    yearsToBeginPeriod d = act360TimeDifferenceInYears (tradeDate d) (beginPeriod d)

    yearsInPeriod d = act360TimeDifferenceInYears (beginPeriod d) (endPeriod d)



class (forall m. Monad m) => OptionMarket a where
    interestRate :: EuropeanOptionDateInfo d => a -> d -> m InterestRate
    underlying   :: EuropeanOptionDateInfo d => a -> d -> m Price
    volatility   :: EuropeanOptionDateInfo d => a -> d -> m Volatility

    europeanOptionPrice :: EuropeanOptionDateInfo d => a -> (Double -> Double) -> d -> m Double
    europeanOptionPrice m payout d =
        let !r           = (interestRateToDouble . interestRate m) d
            !x           = (priceToDouble . underlying m)  d
            !sigma       = (volatilityToDouble . volatility m) d
            !tau         = yearsToMaturity d
            f y          = x * exp ( - (tau * r) ) * (payout . exp) ( - (sigma * sqrt tau * y) + (r-sigma*sigma/2)*tau) * exp (- (y * y / 2)) / sqrt (2 * pi)
            !result      = NI.everywhere NI.trap f
            (!low, !high) = (NI.confidence . NI.absolute 6.0e-10) result
        in (low + high) / 2

