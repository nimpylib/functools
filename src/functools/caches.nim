{.experimental: "callOperator".}

import std/macros

import std/tables
import std/lists
const MultiThrd = compileOption"threads" and not defined(js)
when MultiThrd:
  import std/locks

type
  CacheInfo = tuple[
    hits, misses,
    maxsize, currsize: int
  ]
  Node[K, V] = tuple[
    key: K, result: V,
  ]
  Root[K, V] = DoublyLinkedRing[Node[K, V]]
                            
  Cache[K, V] = ref object
    hits, misses: int
    lock: Lock
    maxsize: int
    full: bool
    cache: Table[K, V]
    root: Root[K, V]

template initAs(res, K, V, tmaxsize){.dirty.} =
  let res = new Cache[K, V]
  res.root = initDoublyLinkedRing[Node[K, V]]()
  res.maxsize = tmaxsize
  when MultiThrd:
    res.lock.initLock()

proc proc_gen_fromImpl(f, cacheName, body: NimNode; maxsize: NimNode): NimNode =

  let f_name = f.name
  let params = f.params

  var ori_name = f_name
  var nname = parseExpr"`()`"
  if f_name.kind == nnkPrefix and f_name[0].eqIdent"*":
    nname = f_name.prefix"*"
    ori_name = f_name[1]

  let name_nocache = genSym(nskProc, ori_name.strVal & "_nocache")

  let
    ResType = params[0]
  let
    tupVal = newNimNode nnkTupleConstr
    ArgsType = newNimNode nnkTupleConstr
    call = newCall name_nocache
  for i in 1..<params.len:
    let p = params[i]
    let ty = p[^2]
    assert ty.kind != nnkEmpty, "please specify type for parameter " & repr p
    for j in 0..<(p.len-2):
      let pj = ident p[j].strVal
      call.add pj
      tupVal.add pj
      ArgsType.add ty

  result = newStmtList()
  f.name = name_nocache
  result.add f
  result.add quote do:
    initAs(`ori_name`, `ArgsType`, `ResType`, `maxsize`)


  let nbody = newStmtList(
    quote do:
      let key{.inject.} = `tupVal`
      template user_function_call(): untyped{.inject.} = `call`
      # template `cacheName`: untyped{.inject.} = `ori_name`
    ,
    body
  )
  let nparams = nnkFormalParams.newTree ResType
  nparams.add newIdentDefs(cacheName, quote do: Cache[`ArgsType`, `ResType`])
  nparams.add params[1..^1]

  result.add nnkProcDef.newTree(nname, f[1], f[2],
    nparams,  # params
    f[4], f[5],
    nbody
  )


# macro proc_gen_from(f; body): untyped = proc_gen_fromImpl(newEmptyNode(), f, body)

macro proc_gen_from(f, cacheName; body): untyped = proc_gen_fromImpl(f, cacheName, body, newLit 0)
macro proc_gen_from(f, cacheName, maxsize; body): untyped = proc_gen_fromImpl(f, cacheName, body, maxsize)


proc cache_info*(self: Cache): CacheInfo =
  ## Report cache statistics
  withLock self.lock:
    return CacheInfo (self.hits, self.misses, self.maxsize, self.cache.len)

proc cache_clear*(self: Cache) =
  ## Clear the cache and cache statistics
  withLock self.lock:
    self.cache.clear()
    self.root.clear()

    self.hits = 0
    self.misses = 0
    self.full = false

template cache_wrapper(user_function; typed: bool): auto =
  # Simple caching without ordering or size limit
  # def wrapper(*args, **kwds):
  #nonlocal hits, misses

  proc_gen_from user_function, res:
    res.cache.withValue(key, valueOfCache):
      result = valueOfCache[]
      res.hits += 1
    do:
      res.misses += 1
      result = user_function_call()
      res.cache[key] = result

template lru_cache_wrapper(user_function; maxsize: int, typed: bool): auto =
  proc_gen_from user_function, res, maxsize:
    let MaxSize = maxsize   # to ensure evaluation only once
    if MaxSize == 0:
      res.misses += 1
      return user_function_call()
    else:
      withLock res.lock:
        res.cache.withValue(key, valueOfCache):
          let link = valueOfCache[]
          # Move the link to the front of the circular queue
          res.root.prepend (key, link)
          res.hits += 1
          return link
        do:
          res.misses += 1

      result = user_function_call()
      withLock res.lock:
        if key in res.cache:
          discard
          #[Getting here means that this same key was added to the
  cache while the lock was released.  Since the link
  update is already done, we need only return the
  computed result and update the count of misses.]#
        elif res.full:
          # Use the old root to store the new key and result.
          let oldroot = res.root.head
          oldroot.value.key = key
          oldroot.value.result = result

          # Empty the oldest link and make it the new root.
          # Keep a reference to the old key and old result to
          # prevent their ref counts from going to zero during the
          # update. That will prevent potentially arbitrary object
          # clean-up code (i.e. __del__) from running while we're
          # still adjusting the links.
          template root: untyped = res.root.head
          root = oldroot.next

          let oldkey = root.value.key
          discard root.value.result
          root.value.key.reset
          root.value.result.reset

          res.cache.del oldkey
          #[Save the potentially reentrant cache[key] assignment
            for last, after the root and links have been put in
            a consistent state.]#
          res.cache[key] = oldroot.value.result
        else:
          res.root.prepend (key, result)
          res.cache[key] = result
          res.full = res.cache.len >= MaxSize


proc lru_cacheImpl(maxsize, typed: NimNode, user_function: NimNode): NimNode =
  ## used as pragma
  let maxsize = quote do:
    if `maxsize` < 0: 0 else: `maxsize`
  result = newCall(bindSym"lru_cache_wrapper", user_function, maxsize, typed)


proc cacheImpl(typed=false, user_function: NimNode): NimNode =
  result = newCall(bindSym"cache_wrapper", user_function, newLit typed)


macro lru_cache*(maxsize:untyped=128, typed:untyped=false, user_function) = lru_cacheImpl(maxsize, typed, user_function)
macro lru_cache*(maxsize:untyped=128, user_function) = lru_cacheImpl(maxsize, newLit false, user_function)
macro lru_cache*(user_function) = lru_cacheImpl(newLit 128, newLit false, user_function)
#macro lru_cache*(maxsize: Option, typed=false, user_function: typed) =

macro cache*(user_function) =
  ## used as pragma
  result = cacheImpl(user_function = user_function)

when isMainModule:
  import std/unittest
  test "t_cache":
    proc f(x: int): int{.cache.} = x+1
    check f(0) == 1
    let time1state = f.cache_info()
    check time1state.hits == 0
    check time1state.misses == 1
    check f(0) == 1
    check f.cache_info().hits == 1
  test "t_lru_cache":
    proc g(x: int): int{.lru_cache(1).} = x+1
    check g(0) == 1
    let time1state = g.cache_info()
    check time1state.hits == 0
    check time1state.misses == 1
    check g(0) == 1
    check g.cache_info().hits == 1

    check g(1) == 2
    check g.cache_info().misses == 2
    check g(0) == 1
    check g.cache_info().hits == 1

