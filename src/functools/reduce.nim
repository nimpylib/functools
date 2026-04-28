
import pkg/pytyping
import pkg/collections_abc

proc reduce*[R, T](function: Callable [[R, T], R], iterable: Iterable[T], initial: R): R =
  result = initial
  for element in iterable:
    result = function(result, element)

proc reduce*[R, T](function: Callable [[R, T], R], iterable: iterator: T): R =
  ## An optimized overload for Nim's [closure iterator](https://nim-lang.org/docs/manual.html#iterators-and-the-for-statement-firstminusclass-iterators)
  result = iterable()
  for element in iterable():
    result = function(result, element)

proc reduce*[R, T](function: Callable [[R, T], R], iterable: Iterable[T]): R =
  for element in iterable:
    once:
      result = element
      continue
    result = function(result, element)

when isMainModule:
  import std/sugar
  let res = reduce((a, b: int) => a + b, [1, 2, 3], 1)
  assert res == 7

  assert reduce((a, b: int) => a + b, [1, 2, 3]) == 6

  iterator myIter(): int{.closure.} =
    yield 1
    yield 2
    yield 3
  
  assert reduce((a, b: int) => a + b, myIter) == 6
