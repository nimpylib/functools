{.experimental: "callOperator".}

# == cmp_to_key ==

#[
type
  Comparable = concept a, b
    a < b is bool
    a <= b is bool
    a == b is bool
    
  Key[T; R: Comparable] = proc (x: T): R{.closure.}
]#

type
  K[T; R: SomeOrdinal] = ref object
    obj: T
    mycmp: proc (x, y: T): R

proc `<`*[T; R](x, y: K[T, R]): bool = x.mycmp(x.obj, y.obj) < R 0
proc `<=`*[T; R](x, y: K[T, R]): bool = x.mycmp(x.obj, y.obj) <= R 0
proc `==`*[T; R](x, y: K[T, R]): bool = x.mycmp(x.obj, y.obj) == R 0


proc `()`*[T; R](k: K[T, R]; obj: T): K[T, R] =
  result = K[T, R]()
  result.mycmp = k.mycmp
  result.obj = obj

proc cmp_to_key*[T, R](mycmp: proc (x, y: T): R): K[T, R] = K[T, R](mycmp: mycmp)

when isMainModule:
  let key = cmp_to_key(proc (x, y: int): int = x - y)
  assert key(5) < key(10)
  assert key(5) <= key(5)
  assert key(5) == key(5)

