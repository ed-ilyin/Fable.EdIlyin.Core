module Fable.EdIlyin.Core.Json.Encode

open Fable.Core.JsInterop


let object fields = createObj fields


let string (x: string) = box x


let int (x: int) = box x


let int64 (x: int64) = box x


let float (x: float) = box x


let bool (x: bool) = box x


let encode (o: obj) = toJson o
