module HyperLogLog where

import Data.Array
import Data.Int
import Data.Bits
import Data.Word

data HLLCounter a = HLLCounter { registers :: Array Int Word8
                               , precision :: Int
                               , hash :: a -> Word64
                               }

-- Initialize a HyperLogLog counter object
emptyHLL :: Int -> (a -> Word64) -> HLLCounter a
emptyHLL p h = HLLCounter {
  registers = listArray (0, (2 ^ p) - 1) (repeat 0)
  , precision = p
  , hash = h
  }

-- Append a new (potentially duplicated) element and increase the
-- counter correspondingly
append :: HLLCounter a -> a -> HLLCounter a
append counter input =
  if (new <= old) then counter
  else HLLCounter { registers = (registers counter) // [(bucket, new)],
                    precision = precision counter,
                    hash = hash counter
                  }
  where
    h = hash counter input
    p = precision counter
    bucket = fromIntegral (h .&. ((2 ^ p) - 1))
    old = (registers counter) ! bucket
    new = fromIntegral ((min (64 - p) (countLeadingZeros h)) + 1)

-- Return an estimate of the number of distinct elemets appended so far
count :: HLLCounter a -> Integer
count counter =
  -- HLL is biased on small cardinalities, use LinearCounting instead
  if fromIntegral chll < 2.5*m then clinear else chll
  where
    m = fromIntegral (length (registers counter))
    chll = countHLL counter
    clinear = countLinear counter

-- Estimate the cardinality using the HLL estimator
countHLL :: HLLCounter a -> Integer
countHLL counter = estimate
  where
    alpha = 0.709
    m = fromIntegral (length (registers counter))
    powers = map ((2 **) . negate . fromIntegral) (elems (registers counter))
    powersum = foldl (+) 0.0 powers
    estimate = round (alpha * m**2 / powersum)

-- Estimate the cardinality using linear counting
countLinear :: HLLCounter a -> Integer
countLinear counter = round (m * log (m/zeros))
  where
    regs = elems (registers counter)
    m = fromIntegral (length regs)
    zeros = (fromIntegral . length . (filter (0 ==))) regs
