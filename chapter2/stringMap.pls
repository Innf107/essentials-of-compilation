export {
    StringMap,
    empty,
    insert,
    lookup,
    values,
    map
}

module List = import("list.pls")

data StringMap(a) =
    < Empty
    , Node({ key : String, value : a, left : StringMap(a), right : StringMap(a) })
    >

let empty : forall a. StringMap(a) = StringMap(Empty)

let insert : forall a. (String, a, StringMap(a)) -> StringMap(a)
let insert(key, value, map) = match map! {
    Empty -> StringMap(Node({ key = key, value = value, left = empty, right = empty }))
    Node(node) -> match compareString(key, node.key) {
        Less -> StringMap(Node({ node with left = insert(key, value, node.left) }))
        Equal -> StringMap(Node({ node with value = value }))
        Greater -> StringMap(Node({ node with right = insert(key, value, node.right) }))
    }
} 

let lookup : forall a. (String, StringMap(a)) -> < Just(a), Nothing >
let lookup(key, map) = match map! {
    Empty -> Nothing
    Node(node) -> match compareString(key, node.key) {
        Less -> lookup(key, node.left)
        Equal -> Just(node.value)
        Greater -> lookup(key, node.right)
    }
}

let values : forall a. StringMap(a) -> List((String, a))
let values(map) = match map! {
    Empty -> []
    Node(node) ->
        List.append(List.append(values(node.left), [(node.key, node.value)]), values(node.right))
}

let map : forall a b. (a -> b, StringMap(a)) -> StringMap(b)
let map(f, node) = match node! {
    Empty -> StringMap(Empty)
    Node(node) -> {
        StringMap(Node({ key = node.key, value = f(node.value), left = map(f, node.left), right = map(f, node.right) }))
    }
}
