import HyperLogLog

import Data.Hash
import Data.Array
import Data.List

main :: IO ()
main = do putStrLn $ intercalate "\n" (map (show . count) hlls)
  where
    xs = take 1000 strings
    hlls = scanl append hllCounter xs

hllCounter :: HLLCounter String
hllCounter = emptyHLL 8 (\x -> fromIntegral (asWord64 (Data.Hash.hash x)))

strings :: [String]
strings = [x ++ [a] | x <- "" : strings, a <- ['a'..'z']]
