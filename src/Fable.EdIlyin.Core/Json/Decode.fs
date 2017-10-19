module Fable.EdIlyin.Core.Json.Decode

open System
open Fable.Core
open Fable.EdIlyin.Core
open Fable.Import
open Fable.EdIlyin.Core.Decode
open Fable.Core.JsInterop


let decodeValue decoder (jsonValue: obj) =
    Decode.decode decoder jsonValue


let decodeString decoder jsonString =
    let pojo = JS.JSON.parse jsonString
    decodeValue decoder pojo


let value : Decode.Decoder<obj,obj> =
    Decode.primitive "a POJO" Ok


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

                | false -> Decode.expectingButGot label o
        )


let at path decoder = List.foldBack field path decoder


[<Emit("typeof $0")>]
let private jsTypeof (o: obj): string = jsNative


let bool =
    let label = "a Bool"

    Decode.primitive label
        (fun (o: obj) ->
            if jsTypeof o = "boolean" then o :?> bool |> Ok
            else Decode.expectingButGot label o
        )


[<Emit("(value => { if (typeof value !== 'number') { return false; }; if (0 <= value && value <= 65535 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isUInt16 (value: obj) : bool = jsNative


[<Emit("(value => { if (typeof value !== 'number') { return false; }; if (0 <= value && value <= 4294967295 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isUInt32 (value: obj) : bool = jsNative


[<Emit("(value => { if (typeof value !== 'number') { return false; }; if (0 <= value && value <= 18446744073709551615 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isUInt64 (value: obj) : bool = jsNative


[<Emit("(value => { if (typeof value !== 'number') { return false; }; if (-32768 <= value && value <= 32767 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isInt16 (value: obj) : bool = jsNative


[<Emit("(value => { if (typeof value !== 'number') { return false; }; if (-2147483648 <= value && value <= 2147483647 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isInt (value: obj) : bool = jsNative


[<Emit("(value => { if (typeof value !== 'number') { return false }; if (-9223372036854775808 <= value && value <= 9223372032559808512 && (value | 0) === value) { return true; } if (isFinite(value) && !(value % 1)) { return true; }; return false; })($0)")>]
let isInt64 (value: obj) : bool = jsNative


let uint16 =
    let label = "an UInt16"
    Decode.primitive label
        (fun (o: obj) ->
            if isUInt16 o then o :?> uint16 |> Ok
            else Decode.expectingButGot label o
        )


let uint32 =
    let label = "an UInt32"
    Decode.primitive label
        (fun (o: obj) ->
            if isUInt32 o then o :?> uint32 |> Ok
            else Decode.expectingButGot label o
        )


let uint64 =
    let label = "an UInt64"
    Decode.primitive label
        (fun (o: obj) ->
            if isUInt64 o then o :?> uint64 |> Ok
            else Decode.expectingButGot label o
        )


let int16 =
    let label = "an Int16"
    Decode.primitive label
        (fun (o: obj) ->
            if isInt16 o then o :?> int16 |> Ok
            else Decode.expectingButGot label o
        )


let int =
    let label = "an Int"
    Decode.primitive label
        (fun (o: obj) ->
            if isInt o then o :?> int |> Ok
            else Decode.expectingButGot label o
        )


let int64 =
    let label = "an Int64"

    Decode.primitive label
        (fun (o: obj) ->
            if isInt64 o then o :?> int64 |> Ok
            else Decode.expectingButGot label o
        )


let float =
    let label = "a Float"

    Decode.primitive label
        (fun (o: obj) ->
            if jsTypeof o = "number" then o :?> float |> Ok
            else Decode.expectingButGot label o
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
                                |> Result.map (tuple name)
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
            if jsTypeof o = "string" then o :?> string |> Ok
            else o |> Decode.expectingButGot label
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
                                |> Result.map (tuple key)
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

                else Decode.expectingButGot label o
            )


let array decoder =
    let label = "a List"

    Decode.primitive label
            (fun (o: obj) ->
                if jsInstanceOfArray o then
                    o
                        :?> obj []
                        |> Array.map (Decode.decode decoder)
                        |> Result.combineArray
                        |> Decode.resultFromResult

                else Decode.expectingButGot label o
            )


let index i decoder =
    Decode.primitive "an array"
        <| fun array ->
            if jsInstanceOfArray array
                then
                    if i >= !!array?length
                        then
                            Decode.expectingButGot
                                (sprintf
                                    "a longer array. Need index %i"
                                    i
                                )
                                array

                        else Decode.run decoder !!array?(i)

                else Decode.expectingButGot "an array" array


[<Emit("$0 === null")>]
let isNull (value: obj) : bool = jsNative


let Null a =
    let label = "a Null"
    Decode.primitive label
        (fun (o: obj) ->
            if isNull o then a |> Ok
            else Decode.expectingButGot label o
        )


let nullable decoder =
    Decode.oneOf [ Null None; Decode.map Some decoder ]


let optionalField fieldName decoder =
    let finishDecoding json =
        match decodeValue (field fieldName value) json with
            | Ok _ -> field fieldName decoder |> Decode.map Some
            | Result.Error _ -> Decode.succeed None

    in value |> Decode.andThen finishDecoding
