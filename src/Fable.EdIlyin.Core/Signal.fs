module Fable.EdIlyin.Core.Signal


type Signal<'a, 'b> =
    { start: ('a -> unit) -> unit; func: ('b -> unit) -> 'a -> unit }


let init start func = { start = start; func = func }


let create func = init func (<|)


let collect func signal =
    init signal.start
        <| fun nextFunc ->
            func >> List.map nextFunc >> ignore |> signal.func


let filter predicate signal =
    init signal.start
        (fun nextFunc ->
            signal.func
                <| fun x -> if predicate x then nextFunc x else ()
        )


let map func signal =
    init signal.start
        <| fun nextFunc -> func >> nextFunc |> signal.func


let start signal = signal.func id |> signal.start
