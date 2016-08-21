namespace CountdownCore

module Solver = 

    open CountdownCore.OptionOperators

    type Operation<'a, 'b> =  
            | Add of 'a * 'b
            | Sub of 'a * 'b
            | Mult of 'a * 'b
            | Div of 'a * 'b

    type Expr = 
            | Num of int 
            | Op of Operation<Expr, Expr>

    let remainingNumbers = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 25, 75, 100]

    let isValid operation = 
        match operation with
        | Add (Num a, Num b) 
        | Mult (Num a, Num b) 
        | Sub (Num a, Num b) 
            when b > a -> false
        | Div (Num a, Num b)
            when b < a -> false
        | _ -> true

    let rec simplify expr =
        match expr with 
            | Num n when n < 1 || n % 1 <> 0 -> None
            | Num n -> Some (n)
            | Op(Add (a, b)) -> add (simplify a) (simplify b)
            | Op(Sub (a, b)) -> sub (simplify a) (simplify b)
            | Op(Mult (a, b)) -> mult (simplify a) (simplify b)
            | Op(Div (a, b)) -> div (simplify a) (simplify b)

    let rec complexify expr number = 
        match expr with
            | Num n -> [Add(expr, number), Sub(expr, number), Div(expr, number), Mult(expr, number)]

    // First simplify and see if it is correct
    // Then complexify to get the next set of solutions
    // Apply a to next set of solutions