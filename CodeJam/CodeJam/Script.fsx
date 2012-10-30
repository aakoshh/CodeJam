
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam2012.fs"


CodeJam.BoxFactory.solve "box-factory-large.in" |> printfn "%s"


type VoidAction = delegate of unit -> unit


let lst = new System.Collections.Generic.List<VoidAction>()

for i in 1 .. 3 do
    lst.Add( (fun () -> printfn "%d" i) )

for f in lst do
    f.Invoke()