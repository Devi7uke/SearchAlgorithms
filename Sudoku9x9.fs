namespace Search

module Sudoku =
    type action =
        | Place
    // Sudoku problem data structure type (Part of the definition of the Sudoku proble)
    type state = int list list
    //Test meta function that evaluates when there is a node with a final valid state (Part of the definition of the Sudoku problem)
    let goal state =
        let checkSubGrid grid = List.sort grid = [1..9]
        let checkRow row = List.sort row = [1..9]
        let checkCol col = List.sort col = [1..9]
        let rows = state
        let cols = List.transpose state
        let grids =
            state
            |> List.chunkBySize 3
            |> List.map (fun chunk -> chunk |> List.transpose |> List.concat)
            |> List.concat
            |> List.chunkBySize 9
        cols |> List.forall checkCol && rows |> List.forall checkRow && grids |> List.forall checkSubGrid
    //Cost of each action
    let cost _ _ _ = 1.0
    //A function that retrieve the position on the state of a empty cell that can be filled
    let findZeroIndex list =
        match list |> List.findIndex (fun list -> list |> List.exists ((=) 0)) with
        | -1 -> (-1, -1)
        | i -> 
            let list' = list.[i]
            match list' |> List.findIndex ((=) 0) with
            | -1 -> (-1, -1)
            | j -> (i, j)
    //The definition of the action that return a state for each candidate for an empty cell
    let successor (i, j) action state =
        let cell = state |> List.item i |> List.item j
        let row = List.item i state
        let col = List.transpose state |> List.item j
        let sgr =
            let rowIndex = (i / 3) * 3
            let colIndex = (j / 3) * 3
            state
            |> List.skip rowIndex
            |> List.take 3
            |> List.map (fun row -> row |> List.skip colIndex |> List.take 3)
            |> List.concat
        let guess = [1..9] |> List.filter (fun cand -> not (row |> List.contains cand || col |> List.contains cand || sgr |> List.contains cand))
        let place = guess |> List.map (fun x -> action, state |> List.mapi (fun i' row -> if i' = i then row |> List.mapi (fun j' cell -> if j' = j then x else cell) else row))
        
        match action with
        | Place ->  if cell = 0 && guess <> [] then place else []
    //successors fucntion that receive an state and return a list of a successors for the current state (Part of the definition of the Sudoku proble)
    let successors state = 
        let zeroIndex = findZeroIndex state
        successor zeroIndex Place state
    //Sudoku problem definition (Part of the definition of the Sudoku proble)
    let problem state = {
        start = state
        successors = successors
        goal = goal
        cost = cost
    }
