import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . factorial . read . head

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
