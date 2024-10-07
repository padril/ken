# Ken

Below is a description of *some possible* future features of ken, and the
direction it's going.

```gleam
io = import "std.io"

// rank Type    = [0]  // scalar
// rank Type[1] = [1]  // rank notation
// rank Type[2] = [2]
// rank Type[n] = [n]  // type variable
// [Type] = Type[1]    // special notation
// [[Type]] = Type[2]  // for vecs and mats

// you can split the type and the definition
fib : Int -> Int
fib x = case x {
    0 -> 0
    1 -> 1
    x -> (fib x - 1) + (fib x - 2)
}

// currying
x add y : Int -> Int -> Int = x + y

// traditional functions using tuples
foo(x, y, z) : (Int, Int, Int) -> Int = x * y + z

struct Tree a {
    root : a
    left : Tree a
    right : Tree a
}

rpt Tree a {
    // ... prove that Tree is rank polymorphic
    // this is the key to ken, and why it needs a dependent type engine
}

tree1 : Tree Int = ...
tree2 : Tree Int = ...
(rank tree1) != (rank tree2)
tree1 add tree2  // works seamlessly

// functions with side effects must be declared by being prefixed by bang
!print str : String -> () = {
    io.stdout <- str  // do non-local assignments with `<-`
    !io.flush  // call side effectful functions with bang
}

!main args : [String] -> () = {
    io.stdout <- "Hello, world!"

    // [1 2 3 5] -- rank polymorphism!
    io.stdout <- (fib [1 2 3 4]) as String

    // 14 -- no operator precedence, just right to left
    io.stdout <- (foo(2, 3, 4)) as String

    /* N.B.
    `as` is just a function
    
    as type : a -> (a -> b)

    Types aren't truly first class, but they can be used as values
    of their own type for the purpose of things like pattern
    matching. This is possible because, for optimization and RPT
    reasons, ken is dependently typed under the hood.
    */
}
```

