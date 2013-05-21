
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam20131A.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils
         
CodeJam.ManageYourEnergy.solve "manageyourenergy-sample.in"
CodeJam.ManageYourEnergy.solve "manageyourenergy-small-practice.in"




