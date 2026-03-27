## ## Why is the 1st argument of `reduce` `ProcCallable`?
## `reduce`_'s first argument `function` is [`Callable` abstract class](https://docs.python.org/3/library/collections.abc.html#collections.abc.Callable) in Python.
##
## Here it's currently `ProcCallable`, meaning we only accept proc type as this Callable, because:
##
## 1. in Nim unless `{.experimental: "callOperator".}` is enabled, only proc is callable.
## 2. I've tried to declare `Callable[T: openArray[typedesc], R] = concept ...` but it just cannot matched proc
##
## ## Why not simply write as `proc (a1: T, a2: R): R`?
## 1. it's too long
## 2. this library is aimed to be look like Python's `functools`, and `ProcCallable [[R, T], R]` is the most similar with `Callable[[R, T], R]`

type
  Iterable[T] = concept self
    T
    for x in self: x is T

import std/macros
macro ProcCallable(TR): typedesc =
  #T: openArray[typedesc], R
  let
    T = TR[0]
    R = TR[1]
  # let argsT = newNimNode nnkArgList
  let argDefs = nnkFormalParams.newTree R
  for i, t in T:
    let a = newIdentDefs(ident("a" & $i), t)
    # argsT.add a
    argDefs.add a

  #result = quote do: proc (`argsT`): `R`
  result = nnkProcTy.newTree argDefs
  result.add newEmptyNode()

proc reduce*[R, T](function: ProcCallable [[R, T], R], iterable: Iterable[T], initial: R): R =
  result = initial
  for element in iterable:
    result = function(result, element)

proc reduce*[R, T](function: ProcCallable [[R, T], R], iterable: iterator: T): R =
  result = iterable()
  for element in iterable():
    result = function(result, element)

proc reduce*[R, T](function: ProcCallable [[R, T], R], iterable: Iterable[T]): R =
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
