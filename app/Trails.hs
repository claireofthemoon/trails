module Trails (fib) where

foreign export ccall fib :: Int -> Int

fib :: Int -> Int
fib 0 = 1
fib n = n * (fib $ n - 1)
