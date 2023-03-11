open Search
let state = [| 1;2;3;4;6;0;7;5;8 |]

//Uniform Cost Search to solve The 8-Puzzle
match Chapter3.treeSearch UCS.strategy (The8Puzzle.problem state) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("
(*
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

//Iterative deepening DFS to solve The 8-Puzzle
match IDS.strategy (The8Puzzle.problem state) with
    | Some n -> 
        let sol = Chapter3.actions n 
        printfn "Solucion: %A" sol
    | None -> printfn "Solucion no encontrada :("
*)