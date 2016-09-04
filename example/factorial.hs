main :: IO ()
main = getLine >>= putStrLn . show . factorial . read

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
