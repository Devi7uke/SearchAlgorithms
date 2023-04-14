namespace Search

module Farmer =
    type action =
        | Duck
        | Corn
        | Fox
        | Farmer
    type state = bool * bool * bool* bool
    //           Duck   Corn   Fox    Farmer
    let start = (false, false, false, false)
    let goal state = state = (true, true, true, true)
    let cost _ _ _ = 1.0
    
    let condition (duck, corn, fox, farmer) = 
        not (farmer <> duck && duck = corn || farmer <> fox && fox = duck)

    let successor action (duck, corn, fox, farmer) =
        match action with
        | Duck -> if condition (not duck, corn, fox, not farmer) then Some (action, (not duck, corn, fox, not farmer)) else None
        | Corn -> if condition (duck, not corn, fox, not farmer) then Some (action, (duck, not corn, fox, not farmer)) else None
        | Fox -> if condition (duck, corn, not fox, not farmer) then Some (action, (duck, corn, not fox, not farmer)) else None
        | Farmer -> if condition (duck, corn, fox, not farmer) then Some (action, (duck, corn, fox, not farmer)) else None

    let successors state =
        [
            successor Duck state
            successor Corn state
            successor Fox state
            successor Farmer state 
        ]   |> List.choose id
    
    let problem = {
        start = start
        successors = successors
        goal = goal
        cost = cost
    }