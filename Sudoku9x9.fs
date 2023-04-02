namespace Search

module Sudoku =
    type action =
        | Place //Place a number once the others have been discarded
        //| Remove //Remove a number for the cell, only to backtracking
        //| Check //Check the consistency of a number placed on a cell based on the Sudoku rules
        //| Discard //Remove a number for the candidates for each cell
        //| Guess //Return a list of candidates for each empty cell
    type state = int list list
    //An example of a beginning state
    let start = [
        [2; 5; 0; 0; 3; 0; 9; 0; 1];
        [0; 1; 0; 0; 0; 4; 0; 0; 0];
        [4; 0; 7; 0; 0; 0; 2; 0; 8];
        [0; 0; 5; 2; 0; 0; 0; 0; 0];
        [0; 0; 0; 0; 9; 8; 1; 0; 0];
        [0; 4; 0; 0; 0; 3; 0; 0; 0];
        [0; 0; 0; 3; 6; 0; 0; 7; 2];
        [0; 7; 0; 0; 0; 0; 0; 0; 3];
        [9; 0; 3; 0; 0; 0; 6; 0; 4]
    ]
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

    let cost _ _ _ = 1.0

    let findZeroIndex list =
        match list |> List.findIndex (fun list -> list |> List.exists ((=) 0)) with
        | -1 -> (-1, -1)
        | i -> 
            let list' = list.[i]
            match list' |> List.findIndex ((=) 0) with
            | -1 -> (-1, -1)
            | j -> (i, j)

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
        //let remove =
        //let check 
        //let discard
        
        match action with
        | Place ->  if cell = 0 && guess <> [] then place else []

    let successors state = 
        let zeroIndex = findZeroIndex state
        successor zeroIndex Place state

    let problem state = {
        start = state
        successors = successors
        goal = goal
        cost = cost
    }
