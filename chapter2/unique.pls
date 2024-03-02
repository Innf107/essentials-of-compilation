export {
    Unique,
    new,
    toInt
}

data Unique = Number

let nextInteger = ref(0)

let new() = {
    let int = nextInteger!
    nextInteger := int + 1
    Unique(int)
}

let toInt : Unique -> Number
let toInt(unique) = unique!