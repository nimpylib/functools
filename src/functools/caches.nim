{.experimental: "callOperator".}

import std/macros
import std/typeinfo
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

template initAs(res, K, V){.dirty.} =
  let res = new Cache[K, V]
  res.root = initDoublyLinkedRing[Node[K, V]]()
  when MultiThrd:
    res.lock.initLock()

proc proc_gen_fromImpl(f, cacheName, body: NimNode): NimNode =
  ## also gen:
  ##  - var cache: Table[K, V]
  ##  - let key: K
  ##  - `template user_function_call(): R`

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
    initAs(`ori_name`, `ArgsType`, `ResType`)


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

macro proc_gen_from(f, cacheName; body): untyped = proc_gen_fromImpl(f, cacheName, body)


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
  #def wrapper(*args, **kwds):
  #nonlocal hits, misses

  proc_gen_from user_function, res:
    res.cache.withValue(key, value):
      result = value[]
      res.hits += 1
    do:
      res.misses += 1
      result = user_function_call()
      res.cache[key] = result

#proc lru_cache_wrapper(user_function: proc, maxsize: int, typed: bool) =


proc cacheImpl(typed=false, user_function: NimNode): NimNode =
  result = newCall(bindSym"cache_wrapper", user_function, newLit typed)


macro cache*(user_function) =
  ## used as pragma
  result = cacheImpl(user_function = user_function)

when isMainModule:
  proc f(x: int): int{.cache.} = x+1
  assert f(0) == 1
  let time1state = f.cache_info
  assert time1state.hits == 0
  assert time1state.misses == 1
  assert f(0) == 1
  assert f.cache_info.hits == 1
  #echo typeof f

