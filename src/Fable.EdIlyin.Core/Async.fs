module Fable.EdIlyin.Core.Async


let (>>=) asyn func = async.Bind (asyn, func)


let (|>>) asyn func = asyn >>= (func >> async.Return)
