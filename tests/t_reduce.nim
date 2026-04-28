
import std/unittest

import functools/reduce

import std/sugar

suite "reduce":
  let res = reduce((a, b: int) => a + b, @[1, 2, 3], 1)
  check res == 7

  check reduce((a, b: int) => a + b, @[1, 2, 3]) == 6

  iterator myIter(): int{.closure.} =
    yield 1
    yield 2
    yield 3
  
  check reduce((a, b: int) => a + b, myIter) == 6

