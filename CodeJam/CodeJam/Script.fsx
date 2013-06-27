
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam20131B.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils
         
CodeJam.GarbledEmail.solve "garbledemail-sample-practice.in"
CodeJam.GarbledEmail.solve "garbledemail-small-practice.in"




