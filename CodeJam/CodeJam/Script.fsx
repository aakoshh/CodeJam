
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam2011R1A.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils

//CodeJam.SafetyInNumbers.solve "box-factory-small.in" |> printfn "%s"

CodeJam.KillerWord.solve "killer-word-sample.in"
            



