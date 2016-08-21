namespace CountdownCore

module OptionOperators =

    let add a b =
        match (a, b) with
            | (Some a, Some b) -> Some (a + b)
            | _ -> None

    let sub a b =
        match (a, b) with
            | (Some a, Some b) -> Some (a - b)
            | _ -> None

    let mult a b =
        match (a, b) with
            | (Some a, Some b) -> Some (a * b)
            | _ -> None

    let div a b =
        match (a, b) with
            | (Some a, Some b) -> Some (a / b)
            | _ -> None