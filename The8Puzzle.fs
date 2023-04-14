namespace Search

module The8Puzzle =
    type action =
        | Left
        | Right
        | Up
        | Down
    type state = int list
    let start = [7;2;4;5;0;6;8;3;1]
    let aim = [1;2;3;4;5;6;7;8;0]
    let cost _ _ _ = 1.0
    let goal state = 
        List.map2 (fun x y -> (x , y)) aim state
        |> List.forall (fun (x, y) -> x = y)
    
    let zero state =
        List.findIndex (fun x -> x = 0) state
    
    let successor i action state =
        let swap i j =
            state
            |> List.mapi (fun idx v -> 
                    if idx = i then 
                        List.item j state
                    else if idx = j then
                        List.item i state 
                    else 
                        v
                )
        match action with
        | Left -> if i % 3 <> 0 then Some (action, swap i (i - 1)) else None
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

    let heuristicOne node =
        List.zip aim node.state
        |> List.sumBy (fun(x, y) -> if x <> y && x <> 0 then 1.0 else 0.0)

    let heuristicTwo node =
        let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
        let distance x =
            let pos = List.findIndex (fun y -> y = x) node.state
            let (row, col) = pos / 3, pos % 3
            let (aim_row, aim_col) = (x - 1) / 3, (x - 1) % 3
            manhattanDistance (row, col) (aim_row, aim_col)
        node.state |> List.sumBy distance |> float

module The8PuzzleMutable =
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
