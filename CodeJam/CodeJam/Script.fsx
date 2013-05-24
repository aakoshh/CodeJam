
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam20131A.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils
         
CodeJam.GoodLuck.solve "goodluck-sample.in"
CodeJam.GoodLuck.solve "goodluck-small-practice-1.in"
CodeJam.GoodLuck.solve "goodluck-small-practice-2.in"




