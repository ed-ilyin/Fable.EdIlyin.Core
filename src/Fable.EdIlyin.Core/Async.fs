module Fable.EdIlyin.Core.Async


let (>>=) asyn func = async.Bind (asyn, func)


let map func asyn = asyn >>= (func >> async.Return)


let (|>>) asyn func = map func asyn
