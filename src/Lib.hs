module Lib
    ( someFunc,
      priceEuropeanCall,
      Strike(..),
      Underlying(..),
      Price(..)
    ) where

import qualified Statistics.Distribution.Normal as Normal
import qualified Data.Time.Clock as Clock

newtype Strike = Strike Double
newtype Underlying = Underlying Double
newtype Price = Price Double
newtype InterestRate = InterestRate Double

priceEuropeanCall :: Normal.NormalDistribution -> Clock.NominalDiffTime -> Strike -> Underlying -> InterestRate -> Price
priceEuropeanCall distr diffTime strike underlying interestRate =
  let (Strike k) = strike
      (Underlying s) = underlying
      (InterestRate r) = interestRate
      d1 = log (s/k)
   in Price d1



someFunc :: IO ()
someFunc = putStrLn "someFunc"
