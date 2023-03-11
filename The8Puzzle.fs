namespace Search

module The8Puzzle =
    type action =
        | Left
        | Right
        | Up
        | Down
    type state = int []
    let start = [| 7;2;4;5;0;6;8;3;1 |]
    let aim = [| 1;2;3;4;5;6;7;8;0 |]
    let cost _ _ _ = 1.0
    let goal state = 
        Array.map2 (fun x y -> (x , y)) aim state
        |> Array.forall (fun (x, y) -> x = y)
    
    let zero state =
        Array.findIndex (fun x -> x = 0) state
    
    let successor i action (state : state) =
        let swap i j =
            let state' = [| for v in state do v |]
            state'.[i] <- state.[j]
            state'.[j] <- state.[i]
            state'
        match action with
        | Left -> if i % 2 <> 0 then Some (action, swap i (i - 1)) else None
        | Right -> if i % 3 <> 2 then Some (action, swap i (i + 1)) else None
        | Up -> if i > 2 then Some (action, swap i ( i - 3)) else None
        | Down -> if i < 6 then Some (action, swap i (i + 3)) else None

    let successors state =
        let index = zero state
        [
            successor index Left state
            successor index Right state
            successor index Up state
            successor index Down state 
        ]   |> List.choose id

    let problem state = {
        start = state
        successors = successors
        goal = goal
        cost = cost
    }