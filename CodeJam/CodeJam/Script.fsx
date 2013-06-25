
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam20131B.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils
         
CodeJam.Osmos.solve "osmos-sample-practice.in"
CodeJam.Osmos.solve "osmos-small-practice.in"
CodeJam.Osmos.solve "osmos-large-practice.in"




