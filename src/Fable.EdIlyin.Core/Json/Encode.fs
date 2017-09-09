module Fable.EdIlyin.Core.Json.Encode

open Fable.Core.JsInterop


let object fields =
    fields
        |> Seq.fold
            (fun o (name: string, value: obj) ->
                do o?(name) <- value
                o
            )
            createEmpty<obj>



let string (x: string) = box x


let int16 (x: int16) = box x


let int (x: int) = box x


let int64 (x: int64) = box x


let uint16 (x: uint16) = box x


let uint32 (x: uint32) = box x


let uint64 (x: uint64) = box x


let float (x: float) = box x


let bool (x: bool) = box x


let encode (o: obj) = toJson o


let Null: obj = null
