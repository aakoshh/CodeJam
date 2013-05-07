
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam2013QR.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils

//CodeJam.SafetyInNumbers.solve "box-factory-small.in" |> printfn "%s"
CodeJam.TicTacToeTomek.solve "tictactoetomek-sample.in"
CodeJam.TicTacToeTomek.solve "tictactoetomek-small-practice.in"
            



