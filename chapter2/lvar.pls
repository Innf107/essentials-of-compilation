module List = import("list.pls")
module StringMap = import("stringMap.pls")
type StringMap(a) = StringMap.StringMap(a)
module Unique = import("unique.pls")
type Unique = Unique.Unique

type Primop = < Read, Add, Subtract >

data LVarExpr(name, self, r) =
    < Int(Number)
    , Prim(Primop, List(LVarExpr(name, r, r)))
    , Var(name)
    , Let(name, LVarExpr(name, r, r), LVarExpr(name, r, r)) 
    | self
    >

data LVarProgram(name) = { body : LVarExpr(name, < > , < >) }

data Env = { variables : StringMap(Number) }

let read() = !cat

let interpretExprWith : forall r. ((Env, LVarExpr(String, r, r)) -> Number, Env, LVarExpr(String, < > , r)) -> Number
let interpretExprWith(recur, env, expr) = {
    let invalidPrimopArgs(primop, exprs) = fail("Invalid arguments to primop '${primop}': " ~ toString(exprs))
    match expr! {
        Let(name, expr, rest) -> {
            let value = recur(env, expr)
            let modifiedEnv = Env({ env! with variables = StringMap.insert(name, value, env!.variables) })
            recur(modifiedEnv, rest)
        }
        Var(name) -> match StringMap.lookup(name, env!.variables) {
            Nothing -> fail("unbound variable: '${name}'")
            Just(value) -> value
        }
        Prim(Read, []) -> parseInt(read())
        Prim(Read, args) -> invalidPrimopArgs("read", args)
        Prim(Add, [arg1, arg2]) -> recur(env, arg1) + recur(env, arg2)
        Prim(Add, args) -> invalidPrimopArgs("+", args)
        Prim(Subtract, [arg1, arg2]) -> recur(env, arg1) + recur(env, arg2)
        Prim(Subtract, args) -> invalidPrimopArgs("-", args)
        Int(int) -> int
    }
}

let interpretExpr : (Env, LVarExpr(String, < > , < >)) -> Number
let interpretExpr(env, expr) = interpretExprWith(interpretExpr, env, expr)


data LVarIf(name, self, r) = LVarExpr(name
                                     , < If({ condition : LVarIf(name, r, r)
                                            , thenBranch : LVarIf(name, r, r)
                                            , elseBranch : LVarIf(name, r, r) 
                                            })
                                       | self > 
                                     , < If({ condition : LVarIf(name, r, r)
                                            , thenBranch : LVarIf(name, r, r)
                                            , elseBranch : LVarIf(name, r, r)
                                            })
                                       | r >)

let interpretLVarIfExprWith : forall r. ((Env, LVarIf(String, r , r)) -> Number, Env, LVarIf(String, < > , r)) -> Number
let interpretLVarIfExprWith(recur, env, expr) = {
    match expr!! {
        If(ifExpr) ->
            if recur(env, ifExpr.condition) != 0 then
                recur(env, ifExpr.thenBranch)
            else
                recur(env, ifExpr.elseBranch)

        other -> interpretExprWith(\env expr -> recur(env, LVarIf(expr)), env, LVarExpr(other))
    }
}

let interpretLVarIfExpr : (Env, LVarIf(String, < > , < >)) -> Number
let interpretLVarIfExpr(env, expr) = interpretLVarIfExprWith(interpretLVarIfExpr, env, expr)


data Name = { original : String, unique : Unique }


type Register = < RSP, RBP, RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15 >

type Arg = < Immediate(Number), Register(Register), Deref(Register, Number) >

type Label = String

data Instruction =
    < AddQ(Arg, Arg)
    , SubQ(Arg, Arg)
    , NegQ(Arg)
    , MovQ(Arg, Arg)
    , PushQ(Arg)
    , PopQ(Arg)
    , CallQ(Label, Number)
    , RetQ
    , Jmp(Label)
    >

data Block = { instructions : List(Instruction) }

data X86Program = { blocks : StringMap(Block) }


type CVarAtom = < Int(Number), Var(Name) >

data CVarExpr = < Atom(CVarAtom), Prim(Primop, List(CVarAtom)) >

data CVarStatement = < Assign(Name, CVarExpr) >

data CVarTail = < Return(CVarExpr), Sequence(CVarStatement, CVarTail) >

data CVar = StringMap(CVarTail)


data Scope = { variables : StringMap(Name) }

let freshName : String -> Name
let freshName(rawName) = Name({ original = rawName, unique = Unique.new() })

let uniquifyExpr : (Scope, LVarExpr(String, < > , < >)) -> LVarExpr(Name, < > , < >)
let uniquifyExpr(scope, expr) = LVarExpr(match expr! {
    Var(rawName) -> match StringMap.lookup(rawName, scope!.variables) {
        Nothing -> fail("Unbound variable: ${rawName}")
        Just(name) -> Var(name)
    }
    Let(rawName, expr, body) -> {
        # lets are non-recursive so we uniquify `expr` in the previous scope
        let expr = uniquifyExpr(scope, expr)
        
        let name = freshName(rawName)
        let scope = Scope({ variables = StringMap.insert(rawName, name, scope!.variables) })
        let body = uniquifyExpr(scope, body)
        Let(name, expr, body)
    }
    Int(int) -> Int(int)
    Prim(primop, arguments) -> Prim(primop, List.map(\expr -> uniquifyExpr(scope, expr), arguments))
})

let uniquify : LVarProgram(String) -> LVarProgram(Name)
let uniquify(LVarProgram(program)) = {
    let initialScope = Scope({ variables = StringMap.empty })
    LVarProgram({ body = uniquifyExpr(initialScope, program.body) })
}

