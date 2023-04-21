namespace Search

module NQueens =
    open System
    open CSP

    type state = int list
    type action = unit

    let start n =
        let rnd = Random()
        [1..n] |> List.map (fun x -> rnd.Next(n))

    let successors state =
        let move col = 
            state
            |> List.mapi (fun i _ -> state |> List.mapi (fun j x -> if j <> col then x else i))
            |> List.filter (fun state' -> state' <> state)
        state
        |> List.mapi (fun col _ -> move col)
        |> List.collect id
        |> List.map (fun state -> ((), state))

    let attacking ((x1 : int, y1 : int), (x2 : int, y2: int)) =
        if y1 = y2 || abs(x2 - x1) = abs(y2 - y1) then 1 else 0
        
    let pairsAttacking state =
        let queens = state |> List.mapi (fun col row -> (col, row))
        queens
        |> List.allPairs queens
        |> List.filter (fun (r1, r2) -> r1 <> r2)
        |> List.sumBy attacking
        |> (fun n -> n / 2)
    
    let goal state =
        pairsAttacking state = 0

    let cost _ _ _ = 1.0 

    let problem n = {
        start = start n
        successors = successors
        goal = goal
        cost = cost
    }

    let csp n =
        let variables = [for i in 0 .. n-1 do string i]
        let domain = [0 .. n-1]
        let constrainT (v1, v2) = {
            v1 = v1
            v2 = v2
            inv = fun state ->
                let dom1 = Map.find v1 state
                let dom2 = Map.find v2 state
                match dom1, dom2 with
                | [value1], [value2] -> attacking ((int v1, value1), (int v2, value2)) = 1
                | _ -> false
        }
        {
            variables = variables
            domains = [for i in 1 .. n do domain]
            constraints = 
                variables
                |> List.allPairs variables
                |> List.filter (fun (c1, c2) -> c1 <> c2)
                |> List.map constrainT
        }
       