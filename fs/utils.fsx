let inline fork f1 f2 x = (f1 x, f2 x)
let getOrDef key def = Map.tryFind key >> Option.defaultValue def
let split (sep: string) (x:string) = x.Split(sep)
let inline revargs f x y = f y x