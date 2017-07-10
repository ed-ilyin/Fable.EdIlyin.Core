module Fable.EdIlyin.Core.Json.Decode

open System
open Fable.Core
open Fable.EdIlyin.Core
open Fable.Import
open Fable.EdIlyin.Core.Decode


let decodeValue decoder (jsonValue: obj) =
    Decode.decode decoder jsonValue


let decodeString decoder jsonString =
    let pojo = JS.JSON.parse jsonString
    decodeValue decoder pojo


let value : Decode.Decoder<obj,obj> =
    Decode.primitive "a POJO" Decode.Decoded


[<Emit("Object.prototype.hasOwnProperty.call($1,$0)")>]
let private jsHasProp (prop: string, o): bool = jsNative


[<Emit("$1[$0]")>]
let private jsGetProp (prop: string, o: obj): 'a = jsNative


let field name decoder =
    let label = sprintf "%s field '%s'" (Decode.getLabel decoder) name

    Decode.primitive label
        (fun o ->
            match jsHasProp (name, o) with
                | true ->
                    jsGetProp (name, o) |> Decode.run decoder

                | false ->
                    Decode.ExpectingButGot (label, sprintf "%A" o)
        )


let at path decoder = List.foldBack field path decoder


[<Emit("typeof $0")>]
let private jsTypeof (o: obj): string = jsNative


let bool =
    let label = "a Bool"

    Decode.primitive label
        (fun (o: obj) ->
            if jsTypeof o = "boolean" then o :?> bool |> Decode.Decoded
            else label => sprintf "%A" o |> Decode.ExpectingButGot
        )


[<Emit("(value => { if (typeof value !== 'number') { return false; }; if (-2147483648 <= value && value <= 2147483647 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isInt (value: obj) : bool = jsNative


let int =
    let label = "an Int"
    Decode.primitive label
        (fun (o: obj) ->
            if isInt o then o :?> int |> Decode.Decoded
            else label => sprintf "%A" o |> Decode.ExpectingButGot
        )


[<Emit("(value => { if (typeof value !== 'number') { return false }; if (-9223372036854775808 <= value && value <= 9223372032559808512 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isInt64 (value: obj) : bool = jsNative


let int64 =
    let label = "an Int64"

    Decode.primitive label
        (fun (o: obj) ->
            if isInt64 o then o :?> int64 |> Decode.Decoded
            else label => sprintf "%A" o |> Decode.ExpectingButGot
        )


let float =
    let label = "a Float"

    Decode.primitive label
        (fun (o: obj) ->
            if jsTypeof o = "number" then o :?> float |> Decode.Decoded
            else label => sprintf "%A" o |> Decode.ExpectingButGot
        )


[<Emit("Object.entries($0)")>]
let private jsObjEntries (o: obj): (string * obj) list = jsNative


let dict decoder =
    let label = getLabel decoder |> sprintf "maping of string to %s"

    value
        |> Decode.andThen
            (fun o ->
                jsObjEntries o
                    |> List.map
                        (fun (name, subObj) ->
                            decode decoder subObj
                                |> Result.map ((=>) name)
                        )
                    |> Result.combineList
                    |> Result.map Map.ofList
                    |> Decode.fromResult
            )


let dateTime =
    float
        |> Decode.map
            (fun i ->
                let start = DateTime(1970,1,1,0,0,0, DateTimeKind.Utc)
                start.AddSeconds i
            )


let string =
    let label = "a String"

    Decode.primitive label
        (fun (o: obj) ->
            if jsTypeof o = "string" then o :?> string |> Decode.Decoded
            else label => sprintf "%A" o |> Decode.ExpectingButGot
        )


let keyValuePairs decoder =
    let label = getLabel decoder |> sprintf "string key %s value pairs"
    value
        |> Decode.andThen
            (fun o ->
                jsObjEntries o
                    |> List.map
                        (fun (key, valueObject) ->
                            decode decoder valueObject
                                |> Result.map ((=>) key)
                        )
                    |> Result.combineList
                    |> Decode.fromResult
            )


[<Emit("$0 instanceof Array")>]
let private jsInstanceOfArray (o: obj): bool = jsNative


let list decoder =
    let label = "a List"

    Decode.primitive label
            (fun (o: obj) ->
                if jsInstanceOfArray o then
                    o
                        :?> obj list
                        |> List.map (Decode.decode decoder)
                        |> Result.combineList
                        |> Decode.resultFromResult

                else label => sprintf "%A" o |> Decode.ExpectingButGot
            )


let index index decoder =
    list value
        |> Decode.andThen
            (fun list ->
                List.tryItem index list
                    |> Result.ofOption
                        "there is no such element in the Array"
                    |> Decode.fromResult
                    |> Decode.andThen
                        (run decoder >> Decode.fromDecodeResult)
            )
