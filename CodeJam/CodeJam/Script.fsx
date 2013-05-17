
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam20131A.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils
         
CodeJam.Bullseye.solve "treasure-sample.in"
CodeJam.Bullseye.solve "treasure-small-practice.in"




