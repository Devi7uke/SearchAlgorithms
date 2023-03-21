open Search
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



(*
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