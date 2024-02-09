type Primop = < Add, Subtract, Read >

data RIntExpr =
    < Int(Number)
    , Prim(Primop, List(RIntExpr))
    >

data RIntProgram(info) = {
    info : info,
    body : RIntExpr
}

let interpretExpr : RIntExpr -> Number
let interpretExpr(expr) = {
    let invalidPrimopArgs(primop, exprs) = fail("Invalid arguments to primop '${primop}': " ~ toString(exprs))

    match expr! {
        Int(int) -> int
        Prim(Add, [expr1, expr2]) -> interpretExpr(expr1) + interpretExpr(expr2)
        Prim(Add, args) -> invalidPrimopArgs("+", args)
        Prim(Subtract, [expr1, expr2]) -> interpretExpr(expr1) - interpretExpr(expr2)
        Prim(Subtract, [expr]) -> 0 - interpretExpr(expr)
        Prim(Subtract, args) -> invalidPrimopArgs("-", args)
        Prim(Read, []) -> parseInt(!cat)
        Prim(Read, args) -> invalidPrimopArgs("read", args)
    }
}

let interpret : RIntProgram(()) -> Number
let interpret(program) = interpretExpr(program!.body)

let partialEvalExpr : RIntExpr -> RIntExpr
let partialEvalExpr(expr) = match expr! {
    Int(int) -> expr
    Prim(Read, args) -> expr
    Prim(Add, [expr1, expr2]) -> match (partialEvalExpr(expr1)!, partialEvalExpr(expr2)!) {
        (Int(x), Int(y)) -> RIntExpr(Int(x + y))
        (evaluatedExpr1, evaluatedExpr2) -> RIntExpr(Prim(Add, [RIntExpr(evaluatedExpr1), RIntExpr(evaluatedExpr2)]))
    }
    Prim(Subtract, [expr1, expr2]) -> match (partialEvalExpr(expr1)!, partialEvalExpr(expr2)!) {
        (Int(x), Int(y)) -> RIntExpr(Int(x - y))
        (evaluatedExpr1, evaluatedExpr2) -> RIntExpr(Prim(Subtract, [RIntExpr(evaluatedExpr1), RIntExpr(evaluatedExpr2)]))
    }
    Prim(Subtract, [expr]) -> match partialEvalExpr(expr)! {
        Int(x) | Int(x) -> RIntExpr(Int(0 - x))
        evaluatedExpr -> RIntExpr(Prim(Subtract, [RIntExpr(evaluatedExpr)]))
    }
    Prim((Add | Subtract) as primop, args) -> fail("Invalid arguments to primop '${toString(primop)}': " ~ toString(args))
}

let partialEval : RIntProgram(()) -> RIntProgram(())
let partialEval(program) = RIntProgram({ program! with body = partialEvalExpr(program!.body) })

exception LexicalError(message : String) = "Lexical error: ${message}"

type Token = 
    < Int(Number)
    , LParen
    , RParen
    , Minus
    , Plus
    , Read
    , EOF
    >

let lex : List(String) -> List(Token)
let lex(chars) = {
    let lexNumber : (List(String), String) -> List(Token)
    let lexNumber(chars, output) = {
        let result(rest) = Int(parseInt(output)) :: lex(rest)
        match chars {
            (("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") as digit) :: rest ->
                lexNumber(rest, output ~ digit)
            _ -> result(chars)
        }
    }

    match chars {
        [] -> []
        (" " | "\t" | "\n") :: rest -> lex(rest)
        "(" :: rest -> LParen :: lex(rest)
        ")" :: rest -> RParen :: lex(rest)
        ("-" :: ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") :: _)
            | (("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") :: _) -> lexNumber(chars, "")
        ("-" :: rest) -> Minus :: lex(rest)    
        ("+" :: rest) -> Plus :: lex(rest)
        ("r" :: "e" :: "a" :: "d" :: rest) -> Read :: lex(rest)
        char :: rest -> fail("Unexpected character: '${char}'")
    }
}
exception ParseError(message : String) = "Parse error: ${message}"

data ParseState = Ref(List(Token))

let peek : ParseState -> Token
let peek(state) = match state!! {
    [] -> EOF
    (token :: _) -> token
}
let next : ParseState -> Token
let next(state) = match state!! {
    [] -> EOF
    (token :: rest) -> {
        state! := rest
        token
    }
}
let advance : ParseState -> ()
let advance(state) = {
    let _ = next(state)
}

let parsePrimop : ParseState -> Primop
let parsePrimop(state) = match next(state){
    Minus -> Subtract
    Plus -> Add
    Read -> Read
    token -> raise ParseError("Unexpected ${toString(token)}. Expected primop")
}

let parseExpr : ParseState -> RIntExpr
let parseExpr(state) = match next(state) {
    Int(int) -> RIntExpr(Int(int))
    LParen -> {
        let primop = parsePrimop(state)
        let parseArguments(state) = match peek(state) {
            RParen -> []
            _ -> parseExpr(state) :: parseArguments(state)
        }
        let arguments = parseArguments(state)

        RIntExpr(Prim(primop, arguments))
    }
    token -> raise ParseError("Unexpected ${toString(token)}. Expected expression")
}

let parseProgram : ParseState -> RIntProgram(())
let parseProgram(state) = {
    let expr = parseExpr(state)
    RIntProgram({ info = (), body = expr })
}

let parse : String -> RIntProgram(())
let parse(code) = {
    let tokens = lex(chars(code))
    parseProgram(ParseState(ref tokens))
}

