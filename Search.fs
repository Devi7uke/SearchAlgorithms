namespace Search

type problem<'s, 'a> = {
    start       :   's
    successors  :   's -> list<'a * 's>
    goal        :   's -> bool
    cost        :   's -> 'a -> 's -> float
}

type node<'s, 'a> = {
    depth       :   int
    path_cost   :   float
    state       :   's
    action      :   option<'a>
    parent      :   option<node<'s, 'a>>
}

type strategy<'s, 'a, 'd> = {
    empty       :   'd
    insert      :   'd -> node<'s, 'a> -> 'd
    remove      :   'd -> option<node<'s, 'a> * 'd>
}

module Chapter3 =
    let mutable expanded_nodes = 0.0

    let expand problem parent = 
        expanded_nodes <- expanded_nodes + 1.0
        problem.successors parent.state
        |> List.map (fun (a, s) -> {
            depth = parent.depth + 1
            state = s
            action = Some a
            parent = Some parent
            path_cost = parent.path_cost + problem.cost parent.state a s
        })

    let treeSearch strategy problem =
        let root = {
            state = problem.start
            depth = 0
            path_cost = 0.0
            action = None
            parent = None
        }

        let fringe = strategy.insert strategy.empty root

        let rec loop fringe =
            match strategy.remove fringe with
            | Some (n, fringe') ->
                if problem.goal n.state then
                    Some n
                else 
                    expand problem n
                    |> List.fold strategy.insert fringe'
                    |> loop
            | None -> None
        loop fringe

    let graphSearch key strategy problem =
        let root = {
            state = problem.start
            depth = 0
            path_cost = 0.0
            action = None
            parent = None
        }

        let fringe = strategy.insert strategy.empty root
        let rec loop (fringe, processed) =
            match strategy.remove fringe with
            | Some (n, fringe') -> 
                if problem.goal n.state then
                    Some n 
                else
                    if Set.contains (key n) processed then 
                        loop (fringe', processed) 
                    else
                        expand problem n
                        |> List.fold strategy.insert fringe'
                        |> (fun fringe -> loop (fringe, Set.add (key n) processed))
            | None -> None
        loop (fringe, Set.empty)

    let rec actions n =
        match n.action, n.parent with
        | Some a, Some p -> actions p @ [a]
        | _ -> []