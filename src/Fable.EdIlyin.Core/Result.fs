namespace Fable.EdIlyin.Core


module Result =
    let map2 fn a b =
        match a, b with
        | Ok a, Ok b -> Ok (fn a b)
        | Error x, _ -> Error x
        | _, Error x -> Error x


    let combineList list =
        Ok List.empty |> List.foldBack (map2 (fun e l -> e::l)) list


    let combineArray array =
        Array.fold
            (map2 (fun a e -> Array.singleton e |> Array.append a))
            (Ok Array.empty)
            array


    let ofOption error option =
        match option with
            | None -> Error error
            | Some x -> Ok x


    let ofChoice choice =
        match choice with
            | Choice1Of2 x -> Ok x
            | Choice2Of2 x -> Error x


    let fromResultResult resultResult =
        match resultResult with
            | Ok (Ok x) -> Ok x
            | Ok (Error x) | Error x -> Error x


    let andThen func result = Result.bind func result


    type Builder () =
        member this.Bind (m, f) = Result.bind f m
        member this.Return x = Ok x
        member this.ReturnFrom m = m
        member this.Zero () = Ok ()


    let unpack errorFunc okFunc =
        function Error e -> errorFunc e | Ok a -> okFunc a


    let unwrap defaultValue okFunc =
        function Error _ -> defaultValue | Ok a -> okFunc a


[<AutoOpen>]
module ResultAutoOpen =
    let result = Result.Builder ()
