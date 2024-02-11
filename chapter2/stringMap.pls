export {
    StringMap,
    empty,
    insert,
    lookup
}

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

