module Fable.EdIlyin.Core.Option


let withDefault def = function None -> def | Some value -> value
