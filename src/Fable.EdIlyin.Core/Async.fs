module Fable.EdIlyin.Core.Async


let andThen func asyn = async.Bind (asyn, func)


let (>>=) asyn func = async.Bind (asyn, func)


let map func asyn = asyn >>= (func >> async.Return)


let (|>>) asyn func = map func asyn
