
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam20131B.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils
         
CodeJam.FallingDiamonds.solve "fallingdiamonds-sample-practice.in"
CodeJam.FallingDiamonds.solve "fallingdiamonds-small-practice.in"
CodeJam.FallingDiamonds.solve "fallingdiamonds-large-practice.in"




