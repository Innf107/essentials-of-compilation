options {
    "--explicate-control"   as explicateControlExamples:   "Print examples for the explicate-control pass"
    "--select-instructions" as selectInstructionsExamples: "Print examples for the select-instructions pass"
    "--assign-homes"        as assignHomesExamples:        "Print examples for the assign-homes pass"
}

module List = import("list.pls")
module StringMap = import("stringMap.pls")
type StringMap(a) = StringMap.StringMap(a)
module Name = import("name.pls")
type NameMap(a) = Name.Map(a)
type Name = Name.Name
module Unique = import("unique.pls")
type Unique = Unique.Unique
module Test = import("test.pls")

type Primop = < Read, Add, Subtract >

data LVarExprR(name, self, r) =
    < Int(Number)
    , Prim(Primop, List(LVarExprR(name, r, r)))
    , Var(name)
    , Let(name, LVarExprR(name, r, r), LVarExprR(name, r, r)) 
    | self
    >
type LVarExpr(name) = LVarExprR(name, < > , < >)

data LVarProgram(name) = { body : LVarExpr(name) }

let intercalateMap : forall a. (String, a -> String, List(a)) -> String
let intercalateMap(separator, pretty, list) = match list {
    [] -> ""
    [x] -> pretty(x)
    (x :: xs) -> pretty(x) ~ separator ~ intercalateMap(separator, pretty, xs)
}

let replicate : (Number, String) -> String
let replicate(count, string) =
    if (count <= 0) then {
        ""
    } else if (mod(count, 2) == 0) then {
        let replicated = replicate(count / 2, string)
        replicated ~ replicated
    } else {
        let replicated = replicate(floor(count / 2), string)
        replicated ~ replicated ~ string
    }

let prettyPrimop : Primop -> String
let prettyPrimop(primop) = match primop {
    Add -> "+"
    Read -> "read"
    Subtract -> "-"
}

let prettyExpr : forall name. (Number, name -> String, LVarExpr(name)) -> String
let prettyExpr(indent, prettyName, expr) = {
    match expr! {
        Int(int) -> toString(int)
        Var(name) -> prettyName(name)
        Prim(primop, arguments) ->
            "(${prettyPrimop(primop)} "
                ~ intercalateMap(" ", \expr -> prettyExpr(indent + 2, prettyName, expr), arguments) ~ ")"
        Let(name, body, rest) -> "(let ([${prettyName(name)}"~" ${prettyExpr(indent + 2, prettyName, body)}])"
            ~ "\n" ~ replicate(indent, " ") ~"${prettyExpr(indent + 2, prettyName, rest)})"
    }
}

let pretty : forall name. (name -> String, LVarProgram(name)) -> String
let pretty(prettyName, program) = prettyExpr(0, prettyName, program!.body)

data Env = { variables : StringMap(Number) }

let read() = !cat

let interpretExprWith : forall r. ((Env, LVarExprR(String, r, r)) -> Number, Env, LVarExprR(String, < > , r)) -> Number
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

let interpretExpr : (Env, LVarExpr(String)) -> Number
let interpretExpr(env, expr) = interpretExprWith(interpretExpr, env, expr)


data LVarIfR(name, self, r) =
    LVarExprR(name
             , < If({ condition : LVarIfR(name, r, r)
                    , thenBranch : LVarIfR(name, r, r)
                    , elseBranch : LVarIfR(name, r, r) 
                    })
               | self > 
             , < If({ condition : LVarIfR(name, r, r)
                    , thenBranch : LVarIfR(name, r, r)
                    , elseBranch : LVarIfR(name, r, r)
                    })
               | r >)
type LVarIf(name) = LVarIfR(name, < > , < >)

let interpretLVarIfExprWith : forall r. ((Env, LVarIfR(String, r , r)) -> Number, Env, LVarIfR(String, < > , r)) -> Number
let interpretLVarIfExprWith(recur, env, expr) = {
    match expr!! {
        If(ifExpr) ->
            if recur(env, ifExpr.condition) != 0 then
                recur(env, ifExpr.thenBranch)
            else
                recur(env, ifExpr.elseBranch)

        other -> interpretExprWith(\env expr -> recur(env, LVarIfR(expr)), env, LVarExprR(other))
    }
}

let interpretLVarIfExpr : (Env, LVarIf(String)) -> Number
let interpretLVarIfExpr(env, expr) = interpretLVarIfExprWith(interpretLVarIfExpr, env, expr)


let prettyName : Name -> String
let prettyName(name) = name!.original ~ "." ~ toString(name!.unique)

type Register = < RSP, RBP, RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15 >

type Arg(arg) = < Immediate(Number), Register(Register), Deref(Register, Number), StackAddress(Number) | arg >

type Label = String

# This uses intel-style ordering, so mov X Y moves the value from Y to X
data Instruction(arg) =
    < AddQ(Arg(arg), Arg(arg))
    , SubQ(Arg(arg), Arg(arg))
    , NegQ(Arg(arg))
    , MovQ(Arg(arg), Arg(arg))
    , PushQ(Arg(arg))
    , PopQ(Arg(arg))
    , CallQ(Label, Number)
    , RetQ
    , Jmp(Label)
    >

data Block(arg) = { instructions : List(Instruction(arg)) }

data X86Program(arg) = { blocks : StringMap(Block(arg)) }

let prettyArg : Arg(< Var(Name) >) -> String
let prettyArg(arg) = match arg {
    Immediate(n) -> toString(n)
    Register(register) -> match register {
        RSP -> "rsp"
        RBP -> "rbp"
        RAX -> "rax"
        RBX -> "rbx"
        RCX -> "rcx"
        RDX -> "rdx"
        RSI -> "rsi"
        RDI -> "rdi"
        R8  -> "r8"
        R9  -> "r9"
        R10 -> "r10"
        R11 -> "r11"
        R12 -> "r12"
        R13 -> "r13"
        R14 -> "r14"
        R15 -> "r15"
    }
    StackAddress(offset) -> "[rsp-${toString(offset*8)}]"
    Var(name) -> prettyName(name)
    Deref(_, _) -> fail("TODO")
}

let prettyInstruction : Instruction(< Var(Name) >) -> String
let prettyInstruction(instruction) = match instruction! {
    AddQ(arg1, arg2) -> "add ${prettyArg(arg1)}, " ~ prettyArg(arg2)
    SubQ(arg1, arg2) -> "sub ${prettyArg(arg1)}, " ~ prettyArg(arg2)
    NegQ(arg)        -> "neg ${prettyArg(arg)}"
    MovQ(arg1, arg2) -> "mov ${prettyArg(arg1)}, " ~ prettyArg(arg2)
    PushQ(arg)       -> "push ${prettyArg(arg)}"
    PopQ(arg)        -> "pop ${prettyArg(arg)}"
    CallQ(label, _)  -> "call ${label}"
    RetQ             -> "ret"
    Jmp(label)       -> "jmp ${label}"
}

let prettyBlock : Block(< Var(Name) >) -> String
let prettyBlock(block) =
    "  " ~ intercalateMap("\n  ", prettyInstruction, block!.instructions)

let prettyX86 : X86Program(< Var(Name) >) -> String
let prettyX86(program) =
    intercalateMap("\n\n", \(label, tail) -> "${label}:\n" ~ prettyBlock(tail), StringMap.values(program!.blocks))


type CVarAtom = < Int(Number), Var(Name) >

type CVarExpr = < Atom(CVarAtom), Prim(Primop, List(CVarAtom)) >

type CVarStatement = < Assign(Name, CVarExpr) >

data CVarTail = < Return(CVarExpr), Sequence(CVarStatement, CVarTail) >

data CVar = StringMap(CVarTail)

let prettyCVarAtom : CVarAtom -> String
let prettyCVarAtom(atom) = match atom {
    Int(int) -> toString(int)
    Var(name) -> prettyName(name)
}

let prettyCVarExpr : CVarExpr -> String
let prettyCVarExpr(expr) = match expr {
    Atom(atom) -> prettyCVarAtom(atom)
    Prim(primop, atoms) -> "(${prettyPrimop(primop)} " ~ intercalateMap(" ", prettyCVarAtom, atoms) ~ ")"
}

let prettyCVarStatement : CVarStatement -> String
let prettyCVarStatement(statement) = match statement {
    Assign(name, expr) -> prettyName(name) ~ " = " ~ prettyCVarExpr(expr)
}

let prettyCVarTail : CVarTail -> String
let prettyCVarTail(tail) = match tail! {
    Return(expr) -> "  return ${prettyCVarExpr(expr)}"
    Sequence(statement, tail) -> "  ${prettyCVarStatement(statement)}\n" ~ prettyCVarTail(tail)
}

let prettyCVar : CVar -> String
let prettyCVar(CVar(map)) = intercalateMap("\n\n", \(label, tail) -> "${label}:\n" ~ prettyCVarTail(tail), StringMap.values(map))


data Scope = { variables : StringMap(Name) }

let uniquifyExpr : (Scope, LVarExpr(String)) -> LVarExpr(Name)
let uniquifyExpr(scope, expr) = LVarExprR(match expr! {
    Var(rawName) -> match StringMap.lookup(rawName, scope!.variables) {
        Nothing -> fail("Unbound variable: ${rawName}")
        Just(name) -> Var(name)
    }
    Let(rawName, expr, body) -> {
        # lets are non-recursive so we uniquify `expr` in the previous scope
        let expr = uniquifyExpr(scope, expr)
        
        let name = Name.fresh(rawName)
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


let rcoExpr : LVarExpr(Name) -> LVarExpr(Name)
let rcoExpr(expr) = {
    let rcoAtom : LVarExpr(Name) -> (LVarExpr(Name), List((Name, LVarExpr(Name))))
    let rcoAtom(expr) = match expr! {
        Var(name) -> (LVarExprR(Var(name)), [])
        Int(int) -> (LVarExprR(Int(int)), [])
        Prim(primop, args) -> {
            let (argAtoms, argBindings) = List.unzipWith(rcoAtom, args)
            let argBindings = List.concat(argBindings)
            
            let primBindingName = Name.fresh("tmp")
            (LVarExprR(Var(primBindingName)), List.append(argBindings, [(primBindingName, LVarExprR(Prim(primop, argAtoms)))]))
        }
        Let(name, expr, rest) -> {
            let (atom, bindings) = rcoAtom(rest)
            let expr = rcoExpr(expr)

            (atom, List.append(bindings, [(name, expr)]))
        }
    }

    LVarExprR(match expr! {
        Var(name) -> Var(name)
        Int(int) -> Int(int)
        Prim(primop, args) -> {
            let (argAtoms, argBindings) = List.unzipWith(rcoAtom, args)
            let argBindings = List.concat(argBindings)

            List.foldr(\(name, body) rest -> LVarExprR(Let(name, body, rest)), LVarExprR(Prim(primop, argAtoms)), argBindings)!
        }
        Let(name, expr, rest) -> Let(name, rcoExpr(expr), rcoExpr(rest))
    })
}

let rcoProgram(LVarProgram(program)) = LVarProgram({ body = rcoExpr(program.body) })

let explicateAssign : (Name, LVarExpr(Name), CVarTail) -> CVarTail
let explicateAssign(name, expr, tail) = match expr! {
    Var(var) -> CVarTail(Sequence(Assign(name, Atom(Var(var))), tail))
    Int(int) -> CVarTail(Sequence(Assign(name, Atom(Int(int))), tail))
    Let(letName, letBody, rest) -> {
        explicateAssign(letName, letBody, explicateAssign(name, rest, tail))
    }
    Prim(primop, arguments) -> {
        let assertAtom(expr) = match expr! {
            (Var(_) | Int(_)) as atom -> atom
            expr -> fail("non-atom in primop argument after rco: ${toString(expr)}")
        }
        CVarTail(Sequence(Assign(name, Prim(primop, List.map(assertAtom, arguments))), tail))
    }
}

let explicateTail : (LVarExpr(Name)) -> CVarTail
let explicateTail(expr) = match expr! {
    Var(name) -> CVarTail(Return(Atom(Var(name))))
    Int(int) -> CVarTail(Return(Atom(Int(int))))
    Let(name, body, rest) ->
        explicateAssign(name, body, explicateTail(rest))
    Prim(primop, arguments) -> {
        let assertAtom(expr) = match expr! {
            (Var(_) | Int(_)) as atom -> atom
            expr -> fail("non-atom in primop argument after rco: ${toString(expr)}")
        }
        CVarTail(Return (Prim(primop, List.map(assertAtom, arguments))))
    }
}

let explicateControl : LVarProgram(Name) -> CVar
let explicateControl(LVarProgram(program)) =
    CVar(StringMap.insert("_start", explicateTail(program.body), StringMap.empty))


if explicateControlExamples then {
    let run(expr) = explicateControl(rcoProgram(uniquify(LVarProgram({ body = LVarExprR(expr) }))))

    print(prettyCVar(run(
        Let("y", LVarExprR(Let("x1", LVarExprR(Int(20)),
                    LVarExprR(Let("x2", LVarExprR(Int(22)),
                        LVarExprR(Prim(Add, [LVarExprR(Var("x1")), LVarExprR(Var("x2"))])))))),
            LVarExprR(Var("y"))))))
} else {}

let atomToArg : CVarAtom -> Arg(< Var(Name) >)
let atomToArg(atom) = match atom {
    Int(int) -> (Immediate(int))
    Var(name) -> (Var(name))
}

let selectExprInstructions : (Arg(< Var(Name) >), CVarExpr) -> List(Instruction(< Var(Name) >))
let selectExprInstructions(target, expr) = {
    let invalidPrimopArgs(name, arguments) = fail("invalid arguments to primop '${name}': " ~ toString(arguments))
    match expr {
        Atom(atom) -> [ Instruction(MovQ(target, atomToArg(atom))) ]
        Prim(Add, [atom1, atom2]) -> {
            let arg1 = atomToArg(atom1)
            let arg2 = atomToArg(atom2)
            if arg1 == target then
                [ Instruction(AddQ(arg1, arg2)) ]
            else if arg2 == target then
                [ Instruction(AddQ(arg2, arg1)) ]
            else
                [ Instruction(MovQ(target, arg1))
                , Instruction(AddQ(target, arg2))
                ]
        }
        Prim(Add, args) -> invalidPrimopArgs("+", args)
        Prim(Subtract, [atom1, atom2]) -> {
            let arg1 = atomToArg(atom1)
            let arg2 = atomToArg(atom2)
            if arg1 == target then
                [ Instruction(SubQ(arg1, arg2)) ]
            else if arg2 == target then
                [ Instruction(SubQ(arg2, arg1))
                , Instruction(NegQ(arg2))
                ]
            else
                [ Instruction(MovQ(target, arg1))
                , Instruction(SubQ(target, arg2))
                ]
        }
        Prim(Subtract, [atom]) -> {
            let arg = atomToArg(atom)
            if arg == target then
                [ Instruction(NegQ(target)) ]
            else
                [ Instruction(MovQ(target, arg))
                , Instruction(NegQ(target))
                ]
        }
        Prim(Subtract, args) -> invalidPrimopArgs("-", args)

        Prim(Read, []) ->
            if target == Register(RAX) then
                [ Instruction(CallQ("read", 0)) ]
            else
                [ Instruction(CallQ("read", 0))
                , Instruction(MovQ(target, Register(RAX)))
                ]
        Prim(Read, args) -> invalidPrimopArgs("read", args)
    }
}

# TODO(polaris): this *doesn't* catch missing Prim cases? (it's type synonyms again, isn't it...)
let selectStatementInstructions : CVarStatement -> List(Instruction(< Var(Name) >))
let selectStatementInstructions(statement) = {
    match statement {
        Assign(name, expr) -> selectExprInstructions(Var(name), expr)
    }
}

let selectTailInstructions : CVarTail -> List(Instruction(< Var(Name) >))
let selectTailInstructions(tail) = match tail! {
    Return(expr) -> selectExprInstructions(Register(RAX), expr)
    Sequence(statement, tail) ->
        List.append(selectStatementInstructions(statement), selectTailInstructions(tail))
}

let selectInstructions : CVar -> X86Program(< Var(Name) >)
let selectInstructions(cvar) = 
    X86Program({ blocks = StringMap.map(\tail -> Block({ instructions = selectTailInstructions(tail) }), cvar!) })


if selectInstructionsExamples then {
    let run(expr) = selectInstructions(explicateControl(rcoProgram(uniquify(LVarProgram({ body = LVarExprR(expr) })))))

    print(prettyX86(run(
        Let("y", LVarExprR(Let("x1", LVarExprR(Int(20)),
                    LVarExprR(Let("x2", LVarExprR(Int(22)),
                        LVarExprR(Prim(Add, [LVarExprR(Var("x1")), LVarExprR(Var("x2"))])))))),
            LVarExprR(Var("y"))))))
} else {}

data AssignHomesEnv = { variableOffsets : NameMap(Number) }

let assignArg : (Ref(AssignHomesEnv), Arg(< Var(Name) >)) -> Arg(< >)
let assignArg(env, arg) = match arg {
    Var(name) -> match Name.lookup(name, env!!.variableOffsets) {
        Just(offset) -> StackAddress(offset)
        Nothing -> {
            let newOffset = Name.size(env!!.variableOffsets)
            env := AssignHomesEnv({ variableOffsets = Name.insert(name, newOffset, env!!.variableOffsets) })
            StackAddress(newOffset)
        }
    }
    arg -> arg
}

let assignHomesInstruction : (Ref(AssignHomesEnv), Instruction(< Var(Name) >)) -> Instruction(< >)
let assignHomesInstruction(env, instruction) = Instruction(match instruction! {
    AddQ(arg1, arg2) -> AddQ(assignArg(env, arg1), assignArg(env, arg2))
    SubQ(arg1, arg2) -> SubQ(assignArg(env, arg1), assignArg(env, arg2))
    NegQ(arg) -> NegQ(assignArg(env, arg))
    MovQ(arg1, arg2) -> MovQ(assignArg(env, arg1), assignArg(env, arg2))
    PushQ(arg) -> PushQ(assignArg(env, arg))
    PopQ(arg) -> PopQ(assignArg(env, arg))
    CallQ(x, y) -> fail("a")
    RetQ -> RetQ
    Jmp(label) -> Jmp(label)
})

let assignHomesBlock : (Ref(AssignHomesEnv), Block(< Var(Name) >)) -> Block(< >)
let assignHomesBlock(env, Block(block)) =
    Block({
            instructions = List.map(\instr -> assignHomesInstruction(env, instr), block.instructions)
          })

let assignHomes : X86Program(< Var(Name) >) -> X86Program(< >)
let assignHomes(X86Program(program)) = {
    let env = ref(AssignHomesEnv({ variableOffsets = Name.emptyMap }))
    X86Program({ blocks = StringMap.map(\block -> assignHomesBlock(env, block), program.blocks) })
}

