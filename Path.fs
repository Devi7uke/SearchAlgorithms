namespace Search

module Path =
    type action =
        | Up
        | Down
        | Left
        | Right
    type state = int list
    let start = [
        0; 0; 0; 0; 0; 0; 0; 0; 
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 1; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0
    ]
    let aim = (2, 2)
    let invalid = [(3, 2); (3, 3); (3, 4); (3, 5); (3, 6); (4, 1); (5, 2); (5, 6)]
    let position state = 
        let index = (state |> List.findIndex (fun x -> x = 1))
        (index / 8, index % 8)
    let goal state = (position state) = aim
    let cost _ _ _ = 1.0

    let successor (i, j) action state =
        let swap i i' =
            state
            |> List.mapi (fun idx x ->
                if idx = i then
                    state |> List.item i'
                elif idx = i' then
                    state |> List.item i
                else 
                    x
            )
        let constraits (i, j) = 
            not (invalid |> List.contains (i, j))

        match action with
        | Left -> if j > 0 && constraits (i, j - 1) then Some (action, swap (i * 8 + j) ((i * 8 + j) - 1)) else None
        | Right -> if j < 7 && constraits (i, j + 1) then Some (action, swap (i * 8 + j) ((i * 8 + j) + 1)) else None
        | Up -> if i > 0 && constraits (i - 1, j) then Some (action, swap (i * 8 + j) ((i * 8 + j) - 8)) else None
        | Down -> if i < 7 && constraits (i + 1, j) then Some (action, swap (i * 8 + j) ((i * 8 + j) + 8)) else None

    let successors state =
        let index = state |> position
        [
            successor index Left state
            successor index Right state
            successor index Up state
            successor index Down state 
        ]   |> List.choose id

    let problem = {
        start = start
        successors = successors
        goal = goal
        cost = cost
    }

    let heuristicOne node =
        let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
        let pos = node.state |> position
        manhattanDistance pos aim |> float

module Path2 =
    type action =
        | Up
        | Down
        | Left
        | Right
    type state = int list

    let start = [
        0; 0; 0; 0; 0; 0; 0; 0; 
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 1; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0;
        0; 0; 0; 0; 0; 0; 0; 0
    ]
    let invalid = [26; 27; 28; 29; 30; 33; 42; 46]
    let goal state = 18 = (state |> List.findIndex (fun x -> x = 1))
    let cost _ _ _ = 1.0

    let successor i action state =
        let swap i j =
            state
            |> List.mapi (fun idx x ->
                if idx = i then
                    state |> List.item j
                elif idx = j then
                    state |> List.item i
                else 
                    x
            )
        match action with
        | Left -> if i % 8 <> 0 && not(List.contains (i - 1) invalid) then Some (action, swap i (i - 1)) else None
        | Right -> if i % 7 <> 0 && not(List.contains (i + 1) invalid) then Some (action, swap i (i + 1)) else None
        | Up -> if i > 7 && not(List.contains (i - 8) invalid) then Some (action, swap i ( i - 8)) else None
        | Down -> if i < 56 && not(List.contains (i + 8) invalid) then Some (action, swap i (i + 8)) else None
    let successors state =
        let index = state |> List.findIndex (fun x -> x = 1)
        [
            successor index Left state
            successor index Right state
            successor index Up state
            successor index Down state 
        ]   |> List.choose id

    let problem = {
        start = start
        successors = successors
        goal = goal
        cost = cost
    }

    let heuristicOne node =
        let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
        let index = List.findIndex (fun x -> x = 1) node.state
        float (manhattanDistance (index / 9, index % 9) (2, 2))