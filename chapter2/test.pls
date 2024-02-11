export {
    TestFailure,
    expectEqual,
    runTests,
}

exception TestFailure(expected : String, actual : String) = "Unhandled test failure: Expected: ${expected}" ~ ", Actual: ${actual}"

let expectEqual(x, y) = {
    if (x == y) then {
        ()
    } else {
        raise TestFailure(toString(x), toString(y))
    }
}

let runTests(cont) = {
    let failed = ref 0
    let total = ref 0

    let section(name, cont) = {
        print("\e[1m${name}\e[0m")
        
        total := total! + 1
        let runTest(name, body) = {
            # exception patterns would be pretty nice actually
            let result = try {
                body()
                Success
            } with {
                TestFailure(expected, actual) -> {
                    Failure("Expected: ${expected}," ~ " Actual: ${actual}")
                }
                error -> Failure("Exception: ${exceptionMessage(error)}")
            }
            match result {
                Success -> {
                    total := total! + 1
                    print("\e[32m- ${name}: passed\e[0m")
                }
                Failure(message) -> print("\e[1m\e[31m- ${name}: FAILED:"~"${message}\e[0m")
            }
        }

        cont(runTest)
    }

    cont(section)
}


