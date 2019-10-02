module Main where

import Control.Monad
import Data.Random.Source.PureMT
import Data.Random
import Control.Monad.State
import Control.Monad.Loops as ML

import Data.Histogram.Fill hiding ( (><) )
import qualified Data.Histogram as H
import Data.Histogram ( Histogram )

import qualified Control.Foldl as F

import           Numeric.LinearAlgebra.HMatrix ( (#>), (><) )
import qualified Numeric.LinearAlgebra.HMatrix as M

import qualified Data.Random.Distribution.MultivariateNormal as G

import Diagrams.Prelude hiding ( normal, sample )
import Diagrams.Backend.Rasterific
import Plots hiding ( numBins, pdf )



main =
  mapM_ print priors




numBins :: Int
numBins = 100

runSampler :: RVar Double -> Int -> Int -> [Double]
runSampler sampler seed n =
  evalState (sample (replicateM n sampler))
             (pureMT (fromIntegral seed))

stats :: (F.Foldable f, Fractional a) =>
         f a -> (a, a, a)
stats v = F.fold stats' v
  where
    stats' = f <$> (F.premap (\x -> x * x) F.sum) <*> F.sum <*> F.genericLength
    f x2Sum xSum n = (var, mean, n)
      where
        mean = xSum / n
        mean2 = x2Sum / n
        var = n * (mean2 - mean * mean) / (n - 1)

hb :: F.Foldable f => f Double -> HBuilder Double (Histogram BinD Double)
hb xs = forceDouble -<< mkSimple (binD lower numBins upper)
  where
    (varX, xBar, _) = stats xs
    lower = xBar - 4.0 * sqrt varX
    upper = xBar + 4.0 * sqrt varX

hist :: F.Foldable f => f Double -> Histogram BinD Double
hist xs = fillBuilder (hb xs) xs


mu0, sigma0 :: Double
mu0 = 0.0
sigma0 = 1.00

priors :: Histogram BinD Double
priors = hist $ runSampler (normal mu0 sigma0) 42 100000

mu :: Double
mu = 0.50

sigma :: Double
sigma = 1.5

likelihood :: Double -> Double -> Double
likelihood x nu = n / d
  where
    n = exp (-(x - nu)^2 / (2 * sigma^2))
    d = sqrt (2 * pi * sigma^2)


ds :: [Double]
ds = runSampler (normal mu sigma) 2 10


posteriorize ::  Histogram BinD Double ->
                 Double ->
                 Histogram BinD Double
posteriorize h x = H.bmap bayes h
  where
    bayes :: Double -> Double -> Double
    bayes theta p = p * likelihood x theta

qs :: [Histogram BinD Double]
qs = scanl posteriorize priors ds

ss :: [Double]
ss = map H.sum qs

ns :: [Histogram BinD Double]
ns = zipWith (\s q -> H.map (/ s) q) ss qs
