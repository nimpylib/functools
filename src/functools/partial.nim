
import std/macros
import std/tables

proc partialImpl(firstArgSelf: static[bool], f: NimNode, args: NimNode): NimNode =
  let fDef = case f.kind
  of nnkSym: f.getImpl
  of nnkClosedSymChoice:
    assert f.len == 1
    f[0].getImpl
  else: f
  let params = fDef.params  # XXX: getImpl causes parameters to be mangled.
  # so we need `canonicalName`

  proc canonicalName(n: NimNode): string =
    ## `` x`genSymN `` -> x
    let s = n.strVal
    let k = s.find('`')
    if k < 0: s else: s[0..<k]

  var flatParams: seq[NimNode] = @[]
  var flatParamNames: seq[string] = @[]
  var flatParamNameNodes: seq[NimNode] = @[]

  const start = when firstArgSelf: 2 else: 1
  for i in start..<params.len:
    let p = params[i]
    let typ = p[^2]
    let defaultVal = p[^1]
    for j in 0..<(p.len - 2):
      let pEle = p[j]
      let pEleName = pEle.strVal
      let argName = genSym(nskParam, pEleName)
      flatParams.add newIdentDefs(argName, typ, defaultVal)
      flatParamNames.add pEle.canonicalName
      flatParamNameNodes.add pEle


  var posArgs: seq[NimNode] = @[]
  var kwPairs: Table[string, NimNode]

  proc check_keyword_acceptable(key: string) =
    var found = false
    for i, n in flatParamNames:
      if n == key:
        if i < posArgs.len:
          error("Argument '" & key & "' already bound positionally in partial", f)
        found = true
        break
    if not found:
      error("partial() got an unexpected keyword argument '" & key & "'", f)

  for arg in args:
    if arg.kind == nnkExprEqExpr:
      let key = arg[0].strVal
      if key.len == 0:
        error("Invalid keyword argument for partial", arg)
      if key in kwPairs:
        error("Keyword argument repeated in partial: '" & key & "'", arg)
      check_keyword_acceptable key
      kwPairs[key] = arg[1]
    else:
      if kwPairs.len > 0:
        error("Positional arguments cannot follow keyword arguments in partial", arg)
      posArgs.add arg

  if posArgs.len > flatParams.len:
    error("Too many positional arguments for partial", f)

  var newParams = @[params[0]]
  var callExpr = newCall(f)
  when firstArgSelf:
    let self = params[1]  # self
    newParams.add self
    callExpr.add self[0]

  for i in posArgs:
    callExpr.add i

  for i in posArgs.len..<flatParams.len:
    let p = flatParams[i]
    var kwValue: NimNode

    let keyName = flatParamNames[i]
    kwPairs.withValue keyName, value:
      kwValue = value[]
    do:
      newParams.add p
      kwValue = p[0]

    callExpr.add nnkExprEqExpr.newTree(flatParamNameNodes[i], kwValue)

  newProc(
    name = newEmptyNode(),
    params = newParams,
    body = callExpr,
    procType = nnkLambda
  )


macro partial*(f: typed, args: varargs[untyped]): proc = partialImpl(false, f, args)
macro partialmethod*(f: typed, args: varargs[untyped]): proc = partialImpl(true, f, args)

when isMainModule:
  proc add(x, y: int): int = x * y
  proc myadd(x, y: int): int = x * y
  let add5 = partial(add, 5)
  assert 50 == add5(10) # Should print 50

  let add52 = partial(myadd, x=5)
  assert 50 == add52(10) # Should print 50*------------------*-

  type O = ref object
  method m(self: O, x, y: int): int{.base.} = x mod y
  let m5 = partialmethod(m, y=5)
  assert O().m5(10) == 0

