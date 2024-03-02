export {
    Name,
    fresh,
    Map,
    emptyMap,
    insert,
    lookup
}

# TODO: This cannot use Unique since transitive imports break the type checker :/
# module Unique = import("unique.pls")


let nextUniqueRef = ref 0

data Name = { original : String, unique : Number }

let fresh : String -> Name
let fresh(rawName) = {
    let unique = nextUniqueRef!
    nextUniqueRef := nextUniqueRef! + 1
    Name({ original = rawName, unique = unique })
}

let compare : (Name, Name) -> < Less, Greater, Equal >
let compare(name1, name2) = {
    if name1!.unique < name2!.unique then
        Less
    else if name1!.unique > name2!.unique then
        Greater
    else
        Equal
}

data Map(a) =
    < Empty
    , Node({ key : Name, value : a, left : Map(a), right : Map(a) })
    >

let emptyMap = Map(Empty)

let insert : forall a. (Name, a, Map(a)) -> Map(a)
let insert(key, value, map) = match map! {
    Empty -> Map(Node({ key = key, value = value, left = emptyMap, right = emptyMap }))
    Node(node) -> match compare(key, node.key) {
        Less -> Map(Node({ node with left = insert(key, value, node.left) }))
        Equal -> Map(Node({ node with value = value }))
        Greater -> Map(Node({ node with right = insert(key, value, node.right) }))
    }
}

let lookup : forall a. (Name, Map(a)) -> < Just(a), Nothing >
let lookup(key, map) = match map! {
    Empty -> Nothing
    Node(node) -> match compare(key, node.key) {
        Less -> lookup(key, node.left)
        Equal -> Just(node.value)
        Greater -> lookup(key, node.right)
    }
}
