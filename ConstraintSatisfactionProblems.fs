namespace Search

module CSP =
    type 'a state = Map<string, 'a list>
    
    type action = unit

    type 'a constrainT = {
        v1      : string
        v2      : string
        inv     : 'a state -> bool
    }

    type 'a csp = {
        variables       : string list
        domains         : 'a list list
        constraints     : 'a constrainT list
    }

    let start csp = 
        List.zip csp.variables csp.domains
        |> Map.ofList
    
    let goal state =
        state
        |> Map.forall (fun v dom -> List.length dom = 1)
    
    let successors csp state =
        let variables = 
            Map.fold (fun vars v dom -> 
                if List.length dom > 1 then v :: vars else vars
            ) [] state
        
        let variable =
            match variables with
            | _ :: _ ->
                variables
                |> List.minBy (fun v -> (List.length (Map.find v state)))
                |> Some
            | [] -> None
        
        match variable with
        | Some v ->
            let dom = Map.find v state
            List.map (fun value -> (), Map.add v [value] state) dom
            |> List.filter (fun (_, state) -> not (List.exists (fun c -> c.inv state) csp.constraints))
        | None -> []

    let problem csp = {
        start = start csp
        successors = successors csp
        goal = goal
        cost = fun _ _ _ -> 1.0
    }

    let backtracking csp =
        problem csp
        |> Chapter3.graphSearch DFS.key DFS.strategy