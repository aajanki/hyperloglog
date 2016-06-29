import HyperLogLog

import Data.Hash
import Data.Array
import Data.List

main :: IO ()
main = do putStrLn $ intercalate "\n" (map (show . count) hlls)
  where
    xs = take 20000 strings
    hlls = scanl append hllCounter xs

hllCounter :: HLLCounter String
hllCounter = emptyHLL 12 (asWord64 . Data.Hash.hash)

strings :: [String]
strings = [x ++ [a] | x <- "" : strings, a <- ['a'..'z']]
