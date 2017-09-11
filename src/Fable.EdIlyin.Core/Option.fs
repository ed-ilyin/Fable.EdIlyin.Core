module Fable.EdIlyin.Core.Option


let withDefault def = function None -> def | Some value -> value


let andMap option functionOption =
    match functionOption, option with
        | Some func, Some value -> func value |> Some
        | _ -> None


let (<*>) functionOption option = andMap option functionOption


//val map2 : mapping:('T1 -> 'T2 -> 'U) -> 'T1 option -> 'T2 option -> 'U option
let map2 mapping option1 option2 =
    Some mapping <*> option1 <*> option2
