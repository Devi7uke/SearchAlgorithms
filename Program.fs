﻿open Search
let statea = [1;2;3;4;0;6;7;5;8]

let state = [
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

let state2 = [
    [0; 7; 0; 0; 4; 5; 6; 0; 3];
    [4; 0; 8; 0; 6; 7; 0; 5; 1];
    [0; 6; 0; 0; 1; 0; 0; 7; 0];
    [3; 0; 7; 9; 0; 0; 1; 8; 0];
    [0; 1; 6; 0; 7; 8; 2; 3; 0];
    [0; 5; 0; 0; 0; 0; 0; 4; 0];
    [6; 0; 0; 0; 2; 0; 4; 0; 0];
    [2; 3; 0; 7; 0; 6; 0; 1; 0];
    [0; 0; 0; 1; 8; 0; 0; 0; 0]
]

let state3 = [
    [0; 2; 0; 0; 0; 0; 0; 0; 9];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 5; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0];
    [0; 8; 0; 0; 0; 0; 0; 6; 0];
    [0; 0; 0; 0; 0; 0; 0; 0; 0]
]

let rec showGrid list = 
    list
    |> List.iter (fun list ->
        list
        |> List.iter (fun i ->
            printf "%A  " i
        )
        printfn ""
    )

let rec newton_Raphson N K b =
    let f = b ** (K + 1.0) + b * (1.0 - N) - 1.0
    let f' = (K + 1.0) * b ** K
    let b' = b - f / f'
    let aux = abs(b' - b) < 0.00001
    match aux with
    | true -> b'
    | false -> newton_Raphson N K b'

let index state = state |> List.findIndex (fun x -> List.exists (fun y -> y = 0) x)

let t0 = System.DateTime.UtcNow
//match Chapter3.treeSearch UCS.strategy (Sudoku.problem state2) with
//match Chapter3.treeSearch BFS.strategy (Sudoku.problem state2) with
match Chapter3.treeSearch DFS.strategy (Sudoku.problem state) with
    |Some n ->
        let sol = Chapter3.actions n
        let N = Chapter3.expanded_nodes
        let K = n.depth
        let b = 1.0
        //printfn "Depth: %A\nActions: %A\n" n.depth sol
        printfn "Expanded nodes: %A \nAverage branching factor: %A" N (newton_Raphson N K b)
        printfn "Solution: \n"
        showGrid n.state
    |None -> printfn "There is no solution :("
let delta = System.DateTime.UtcNow - t0
printfn "Time elapsed: %A" delta

(*
match Chapter3.treeSearch DFS.strategy (The8Puzzle.problem statea) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("
*)
(*
let Arraystate = [| 1;2;3;4;6;0;7;5;8 |]
let state = [1;2;3;4;0;6;7;5;8]
let state_26 = [5;7;3;6;4;2;0;8;1]
let state_20 = [7;2;4;5;0;6;8;3;1]
let statex = [1;7;0;4;6;2;3;8;5]

let t0 = System.DateTime.UtcNow
match Chapter3.graphSearch (AAS.key The8Puzzle.heuristicTwo) (AAS.strategy The8Puzzle.heuristicTwo) (The8Puzzle.problem state_20) with
    | Some n ->
        let sol = Chapter3.actions n
        printfn "Solution %A \n Depth: %A" sol (List.length sol)
    | None -> printfn "There is no solution for this problem :("
let delta = System.DateTime.UtcNow - t0
printfn "Time elapsed: %A" delta
//Iterative deepening DFS to solve The 8-Puzzle
match IDSUCS.strategy (The8Puzzle.problem statex) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("

match Chapter3.treeSearch (DFSL.strategy 30) (The8Puzzle.problem listState) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("

//Uniform Cost Search to solve The 8-Puzzle
match Chapter3.treeSearch UCS.strategy (The8Puzzle.problem listState) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("

match Chapter3.treeSearch UCS.strategy (The8Puzzle.problem listState) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("

//Breadth First Search to solve The 8-Puzzle
match Chapter3.treeSearch BFS.strategy (The8Puzzle.problem state) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("

//Depth First Search to solve The 8-Puzzle
match Chapter3.treeSearch DFS.strategy (The8Puzzle.problem state) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("

//Depth First Search Limited to solve The 8-Puzzle
match Chapter3.treeSearch (DFSL.strategy 3) (The8Puzzle.problem state) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("


*)