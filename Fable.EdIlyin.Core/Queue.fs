module Fable.EdIlyin.Core.Queue


type 'a queue = | Queue of 'a list * 'a list


let empty = Queue ([], [])


let singleton item = Queue ([item], [])


let push (Queue (l, r)) item = Queue (l, item :: r)


let ofList list = Queue (list, [])


let rec peek (Queue (l, r)) =
    match l with
        | [] ->
            match r with
                | [] -> None
                | _ -> List.rev r |> ofList |> peek
                
        | h::_ -> Some h


let rec pull (Queue (l, r)) =
    match l with
        | [] ->
            match r with
                | [] -> None
                | _ -> List.rev r |> ofList |> pull

        | h::t -> Some (h, Queue (t, r))

 
let length (Queue (l, r)) = List.length l + List.length r
