import Data.Array
import Data.Int
import Data.Bits

data HLLCounter a = HLLCounter { registers :: Array Int Int8
                               , hash :: a -> Int64
                               }

emptyHLL :: (a -> Int64) -> HLLCounter a
emptyHLL h = HLLCounter { registers = listArray (0, 16383) (repeat 0), hash = h }

append :: HLLCounter a -> a -> HLLCounter a
append counter input =
  if (new <= old) then counter
  else HLLCounter { registers = (registers counter) // [(bucket, new)], hash = hash counter }
  where
    h = hash counter input
    bucket = fromIntegral (h .&. 0x3fff)
    old = (registers counter) ! bucket
    new = (min 50 (fromIntegral (countLeadingZeros h))) + 1

count :: HLLCounter a -> Integer
count counter = estimate
  where
    alpha = 0.709
    m = fromIntegral (length (registers counter))
    powers = map ((2 **) . fromIntegral) (elems (registers counter))
    powersum = foldl (+) 0.0 powers
    estimate = round (alpha * (m**2) * powersum)
