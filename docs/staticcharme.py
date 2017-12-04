###
### Chapter 14: Interpreters
###
### StaticCharme Interpreter Code
###
### David Evans, March 2009
###

def tokenize(s):
    current = '' 
    tokens = [] 
    for c in s: 
        if c.isspace(): 
            if len(current) > 0:
                tokens.append(current) 
                current = '' 
        elif c in '():': # added : for type declarations to avoid need for space
            if len(current) > 0: # end the current token 
                tokens.append(current)
                current = ''
            tokens.append(c) 
        else: 
            current = current + c 
    # end of the for loop (by indentation); reached the end of s
    if len(current) > 0: 
        tokens.append(current)
    return tokens

def parse(s):
    def parsetokens(tokens, inner):
       res = []
       while len(tokens) > 0:
          current = tokens.pop(0)
          if current == '(':
             res.append (parsetokens(tokens, True))
          elif current == ')':
             if inner:
                return res
             else:
                error('Unmatched close paren: ' + s)
                return None
          else:
             res.append(current)
        
       if inner:
          error ('Unmatched open paren: ' + s)
          return None
       else:
          return res

    return parsetokens(tokenize(s), False)

###
### Evaluator
###

def meval(expr, env):
    if isPrimitive(expr):
       return evalPrimitive(expr)
    elif isIf(expr):             
        return evalIf(expr, env) 
    elif isDefinition(expr):                
       evalDefinition(expr, env)
    elif isName(expr):
       return evalName(expr, env)
    elif isLambda(expr):
       return evalLambda(expr, env)
    elif isApplication(expr):
       return evalApplication(expr, env)
    else:
       error ('Unknown expression type: ' + str(expr))

### Primitives

def isPrimitive(expr):
    return (isNumber(expr) or isPrimitiveProcedure(expr))

def isNumber(expr):
    return isinstance(expr, str) and expr.isdigit()

def evalPrimitive(expr):
    if isNumber(expr): return int(expr)
    else: return expr

def isPrimitiveProcedure(expr):
    return callable(expr)

def primitivePlus (operands):
    if (len(operands) == 0): return 0
    else: return operands[0] + primitivePlus (operands[1:])

def primitiveTimes (operands):
    if (len(operands) == 0): return 1
    else: return operands[0] * primitiveTimes (operands[1:])
    
def primitiveMinus (operands):
    if (len(operands) == 1):
       return -1 * operands[0]
    elif len(operands) == 2:
       return operands[0] - operands[1]
    else:
       evalError('- expects 1 or 2 operands, given %s: %s' % (len(operands), str(operands)))

def primitiveEquals (operands):
    checkOperands (operands, 2, '=')
    return operands[0] == operands[1]

def primitiveZero (operands):
    checkOperands (operands, 1, 'zero?')
    return operands[0] == 0

def primitiveGreater (operands):
    checkOperands (operands, 2, '>')
    return operands[0] > operands[1]

def primitiveLessThan (operands):
    checkOperands (operands, 2, '<')
    return operands[0] < operands[1]

def checkOperands(operands, num, prim):
    if (len(operands) != num):
       evalError('Primitive %s expected %s operands, given %s: %s' 
                 % (prim, num, len(operands), str(operands)))

### Special Forms
       
def isSpecialForm(expr, keyword):
    return isinstance(expr, list) and len(expr) > 0 and expr[0] == keyword

def isIf(expr):
    return isSpecialForm(expr, 'if')

def evalIf(expr,env):
    assert isIf(expr)
    if len(expr) != 4:
        evalError ('Bad if expression: %s' % str(expr))
    if meval(expr[1], env) != False:
        return meval(expr[2],env)
    else:
        return meval(expr[3],env)

### Definitions and Names

class Environment:
    def __init__(self, parent):
        self._parent = parent
        self._frame = { }
    def addVariable(self, name, typ, value): # added typ parameter
        self._frame[name] = (typ, value) # tuple instead of value
    def _lookupPlace(self, name): # added
        if self._frame.has_key(name): return self._frame[name]
        elif (self._parent): return self._parent._lookupPlace(name)
        else: return None
    def lookupVariable(self, name): # rewritten to use _lookupPlace
        return self._lookupPlace(name)[1]
    def lookupVariableType(self, name): # added
        place = self._lookupPlace(name)
        if place: return place[0]
        else: return CErrorType("Name not found: " + name)
    def reverseLookupType(self, val): # added to lookup primitive procs
        for entry in self._frame.itervalues():
            if entry[1] == val: return entry[0]
        return CErrorType("Value not found")  
        
def isDefinition(expr):
    return isSpecialForm(expr, 'define')

def evalDefinition(expr, env):
    assert isDefinition(expr)
    if len(expr) != 5: evalError('Bad definition: ' + str(expr))
    if expr[2] != ':': evalError('Definition missing type: ' + str(expr))
    env.addVariable(expr[1], parseType(expr[3]), meval(expr[4], env))       

def isName(expr):
    return isinstance(expr, str)

def evalName(expr, env):
    assert isName(expr)
    return env.lookupVariable(expr)

### Procedures

class Procedure:
    def __init__(self, params, typ, body, env): # added typ
        self._params = params
        self._body = body
        self._typ = typ # added
        self._env = env
    def getParamTypes(self): # added
        return self._typ
    def getParams(self):
        return self._params
    def getBody(self):
        return self._body
    def getEnvironment(self):
        return self._env        
    def __str__(self):
        return '<Procedure %s / %s>' % (str(self._params), str(self._body))

def isLambda(expr):
    return isSpecialForm(expr, 'lambda')

#def evalLambda(expr,env):
#    assert isLambda(expr)
#    if len(expr) != 3:
#        evalError ('Bad lambda expression: %s' % str(expr))
#    return Procedure(expr[1], expr[2], env)

def evalLambda(expr, env):
    assert isLambda(expr)
    assert len(expr) == 3
    params = expr[1]
    paramtypes = []
    paramnames = []
    assert len(params) % 3 == 0
    for i in range(0, len(params) / 3):
        name = params[i * 3]
        paramnames.append(name)
        typ = parseType(params[(i * 3) + 2])
        paramtypes.append(typ)
    return Procedure(paramnames, paramtypes, expr[2], env)

### Applications
                   
def isApplication(expr): # requires: all special forms checked first
    return isinstance(expr, list)
   
def evalApplication(expr, env):
    subexprs = expr
    subexprvals = map (lambda sexpr: meval(sexpr, env), subexprs)
    return mapply(subexprvals[0], subexprvals[1:])

def mapply(proc, operands):
    if (isPrimitiveProcedure(proc)):
        return proc(operands)
    elif isinstance(proc, Procedure):
        params = proc.getParams()
        types = proc.getParamTypes() # added
        newenv = Environment(proc.getEnvironment())
        if len(params) != len(operands):
            evalError ('Parameter length mismatch: %s given operands %s' 
                       % (str(proc), str(operands)))
        for i in range(0, len(params)):
            newenv.addVariable(params[i], types[i], operands[i]) # added type
        return meval(proc.getBody(), newenv)        
    else:
        evalError('Application of non-procedure: %s' % (proc))

### Eval-Loop

def typeFromString(s):
    p = parse(s)
    assert len(p) == 1
    return parseType(p[0])

def initializeGlobalEnvironment(): # added types
    global globalEnvironment                  
    globalEnvironment = Environment(None)
    globalEnvironment.addVariable('true', CPrimitiveType('Boolean'), True)
    globalEnvironment.addVariable('false', CPrimitiveType('Boolean'), False)                   
    globalEnvironment.addVariable \
      ('+', typeFromString('((Number Number) -> Number)'), primitivePlus)
    globalEnvironment.addVariable \
      ('-', typeFromString('((Number Number) -> Number)'), primitiveMinus)
    globalEnvironment.addVariable \
      ('*', typeFromString('((Number Number) -> Number)'), primitiveTimes)
    globalEnvironment.addVariable \
      ('=', typeFromString('((Number Number) -> Boolean)'), primitiveEquals)
    globalEnvironment.addVariable \
      ('<', typeFromString('((Number Number) -> Boolean)'), primitiveLessThan)
    
def evalError(msg): # not in book
    print "Error: " + msg    

def parseError(msg): # not in book
    print "Parse Error: " + msg    

def evalLoop():
    initializeGlobalEnvironment()
    while True:
        inv = raw_input('StaticCharme> ')
        if inv == 'quit': break
        for expr in parse(inv):
           typ = typecheck(expr, globalEnvironment) # added 
           if typ and typ.isError(): # added
               print 'Error: ' + typ.getMessage()
           else:
               res = meval(expr, globalEnvironment)
               if res != None: print str(res)

###
### Defining Types
####

class CType:
    def isPrimitiveType(self): return False
    def isProcedureType(self): return False
    def isProductType(self): return False
    def isError(self): return False

class CPrimitiveType(CType):
    def __init__(self, s): self._name = s
    def __str__(self): return self._name
    def isPrimitiveType(self): return True
    def matches(self, other):
        return other.isPrimitiveType() and self._name == other._name

class CProcedureType(CType):
    def __init__(self, args, rettype):
        self._args = args
        self._rettype = rettype
    def __str__(self):
        return '(' + str(self._args) + ' -> ' + str(self._rettype) + ')'
    def isProcedureType(self): return True
    def getReturnType(self): return self._rettype
    def getParameterTypes(self): return self._args
    def matches(self, other):
        return other.isProcedureType() \
               and self._args.matches(other._args) \
               and self._rettype.matches(other._rettype)

class CProductType(CType):
    def __init__(self, types): self._types = types
    def __str__(self):
        res = '('
        firstone = True
        for t in self._types:
            if firstone: firstone = False
            else: res = res + ' '
            res = res + str(t)
        res = res + ')'
        return res
    def isProductType(self): return True    
    def matches(self, other):
        if other.isProductType():
            st = self._types
            ot = other._types
            if len(st) == len(ot): # number of types must match
                for i in range(0, len(st)):
                    if not st[i].matches(ot[i]): return False
                # reached end of loop; all matched so types match
                return True
        return False

class CErrorType(CType):
    def __init__(self, msg): self._msg = msg
    def __str__(self): return '<Type Error: ' + self._msg + '>'
    def getMessage(self): return self._msg
    def matches(self, other): return False
    def isError(self): return True

###
### Constructing Types
###

def parseType(p):
    if isinstance(p, str):
        if p == 'Number': return CPrimitiveType('Number')
        elif p == 'Boolean': return CPrimitiveType('Boolean')
        else: evalError('Undefined type: ' + p)
    else:
        if len(p) == 3 and p[1] == '->':
            return CProcedureType(parseType(p[0]), parseType(p[2]))
        else: # must be product type
            return CProductType(map(parseType, p))

### Type Checking
        
def typecheck(expr, env):
    if isPrimitive(expr): return typePrimitive(expr)
    elif isIf(expr): return typeIf(expr, env)
    elif isLambda(expr): return typeLambda(expr, env)
    elif isDefinition(expr): return typeDefinition(expr, env)
    elif isName(expr): return typeName(expr, env)
    elif isApplication(expr): return typeApplication(expr, env)
    else: evalError ("Unknown expression type: " + str(expr))

def typePrimitive(expr):
    if isNumber(expr): return CPrimitiveType('Number')
    elif isinstance(expr, bool): return CPrimitiveType('Boolean')
    elif callable(expr): return globalEnvironment.reverseLookupType(expr)
    else: assert False

def typeIf(expr, env):
    if len(expr) != 4: evalError('Badly formed if: ' + str(expr))
    # Predicate must be a Boolean
    predtype = typecheck(expr[1], env)
    if not CPrimitiveType('Boolean').matches(predtype):
        return CErrorType('Mistyped predicate: ' + str(predtype))
    constype = typecheck(expr[2], env)
    alttype = typecheck(expr[3], env)
    if not constype.matches(alttype):
        return CErrorType('Inconsistent branch types: %s, %s' % (constype, alttype))
    return constype
                          
def typeName(expr, env):
    return env.lookupVariableType(expr)

def typeDefinition(expr, env):
    assert isDefinition(expr)
    if len(expr) != 5: evalError ('Bad definition: %s' % str(expr))
    name = expr[1]
    if isinstance(name, str):
        if expr[2] != ':': return CErrorType ('Definition missing type: %s' % str(expr))        
        typ = parseType(expr[3])
        newenv = Environment(env)
        newenv.addVariable(name, typ, None)
        etyp = typecheck(expr[4], newenv)
        if not typ.matches(etyp):
            return CErrorType \
              ('Mistyped definition: %s declared type %s, actual type %s'
               % (name, typ, etyp))
        return None # definition has no type
    else:
        return CErrorType ("Bad definition: %s" % str(expr))

def typeApplication(expr, env):
    proctype = typecheck(expr[0], env)
    if not proctype.isProcedureType():
        return CErrorType('Application of non-procedure: ' + str(expr[0]))
    optypes = map (lambda op: typecheck(op, env), expr[1:])
    optype = CProductType(optypes)
    if not proctype.getParameterTypes().matches(optype):
        return CErrorType('Parameter type mismatch: expected %s, given %s'
                               % (proctype.getParameterTypes(), optype))
    return proctype.getReturnType()

def typeLambda(expr, env):
    assert isLambda(expr)
    if len(expr) != 3: evalError ('Bad lambda expression: %s' % str(expr))
    newenv = Environment(env)
    params = expr[1]
    paramnames = []
    paramtypes = []
    assert len(params) % 3 == 0
    for i in range(0, len(params) / 3):
        assert params[(i*3)+1] == ':'
        name = params[i*3]
        typ = parseType(params[(i*3)+2])
        paramnames.append(name)
        paramtypes.append(typ)
        newenv.addVariable(name, typ, None)
    resulttype = typecheck(expr[2], newenv)
    return CProcedureType(CProductType(paramtypes), resulttype)
