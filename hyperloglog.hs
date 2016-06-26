import Data.Array
import Data.Int
import Data.Bits

data HLLCounter a = HLLCounter { registers :: Array Int Int8
                               , precision :: Int
                               , hash :: a -> Int64
                               }

emptyHLL :: Int -> (a -> Int64) -> HLLCounter a
emptyHLL p h = HLLCounter { registers = listArray (0, (2 ^ p) - 1) (repeat 0), precision = p, hash = h }

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

count :: HLLCounter a -> Integer
count counter = estimate
  where
    alpha = 0.709
    m = fromIntegral (length (registers counter))
    powers = map ((2 **) . fromIntegral) (elems (registers counter))
    powersum = foldl (+) 0.0 powers
    estimate = round (alpha * m**2 / powersum)
