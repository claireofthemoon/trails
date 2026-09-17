module Trails (fib) where

foreign export ccall fib :: Int -> Int

-- I know this isn't Fibonacci - just avoiding recursion for now and updating the name is annoying
fib :: Int -> Int
fib n = n * 2
