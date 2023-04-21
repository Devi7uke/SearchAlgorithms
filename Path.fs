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
