
import std/unittest

import functools

test "partial":
  proc add(x, y: int): int = x * y
  proc myadd(x, y: int): int = x * y
  let add5 = partial(add, 5)
  check 50 == add5(10) # Should print 50

  let add52 = partial(myadd, x=5)
  check 50 == add52(10) # Should print 50*------------------*-

type O = ref object
method m(self: O, x, y: int): int{.base.} = x mod y

test "partialmethod":
  let m5 = partialmethod(m, y=5)
  check O().m5(10) == 0

test "cmp_to_key":
  let key = cmp_to_key(proc (x, y: int): int = x - y)
  check key(5) < key(10)
  check key(5) <= key(5)

