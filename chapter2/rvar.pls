
type Primop = < Read, Add, Subtract >

type Name = String

data RVarExpr(self, r) =
    < Int(Number)
    , Prim(Primop, List(RVarExpr(r, r)))
    , Var(Name)
    , Let(Name, RVarExpr(r, r), RVarExpr(r, r)) 
    | self
    >


data RVarProgram(r) = { body: RVarExpr(r, r) }

let read() = !cat

let interpretExprWith : forall r. (RVarExpr(r, r) -> Number, RVarExpr(< > , r)) -> Number
let interpretExprWith(recur, expr) = {
    let invalidPrimopArgs(primop, exprs) = fail("Invalid arguments to primop '${primop}': " ~ toString(exprs))
    match expr! {
        Let(_, _, _) -> fail("TODO")
        Var(_) -> fail("TODO")
        Prim(Read, []) -> parseInt(read())
        Prim(Read, args) -> invalidPrimopArgs("read", args)
        Prim(Add, [arg1, arg2]) -> recur(arg1) + recur(arg2)
        Prim(Add, args) -> invalidPrimopArgs("+", args)
        Prim(Subtract, [arg1, arg2]) -> recur(arg1) + recur(arg2)
        Prim(Subtract, args) -> invalidPrimopArgs("-", args)
        Int(int) -> int
    }
}

let interpretExpr : RVarExpr(< > , < >) -> Number
let interpretExpr(expr) = interpretExprWith(interpretExpr, expr)


data RVarIf(self, r) = RVarExpr(< If({ condition : RVarIf(r, r), thenBranch : RVarIf(r, r), elseBranch : RVarIf(r, r) }) | self > 
                              , < If({ condition : RVarIf(r, r), thenBranch : RVarIf(r, r), elseBranch : RVarIf(r, r) }) | r >)

let interpretRVarIfExprWith : forall r. (RVarIf(r , r) -> Number, RVarIf(< > , r)) -> Number
let interpretRVarIfExprWith(recur, expr) = {
    match expr!! {
        If(ifExpr) ->
            if recur(ifExpr.condition) != 0 then
                recur(ifExpr.thenBranch)
            else
                recur(ifExpr.elseBranch)

        other -> interpretExprWith(\expr -> recur(RVarIf(expr)), RVarExpr(other))
    }
}

let interpretRVarIfExpr : RVarIf(< > , < >) -> Number
let interpretRVarIfExpr(expr) = interpretRVarIfExprWith(interpretRVarIfExpr, expr)
