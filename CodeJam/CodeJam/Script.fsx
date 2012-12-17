
#load "CodeJamUtils.fs"
# nowarn "25" // don't warn on incomplete pattern matches
# nowarn "40"
#load "CodeJam2012R1B.fs"

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)

open CodeJam.Utils

//CodeJam.SafetyInNumbers.solve "box-factory-small.in" |> printfn "%s"

CodeJam.EqualSums.solve "equal-sums-small.in"
                
let H,F,C = CodeJam.Tide.toCase 
                "405 7 10
1000 615 1000 996 1000 251 1000 995 1000 589
910 102 522 449 925 602 944 399 818 373
900 288 897 300 199 959 467 915 443 95
907 671 830 665 396 199 615 890 837 52
933 724 912 109 764 772 556 743 648 613
851 261 831 646 468 113 927 341 680 702
1000 996 1000 533 1000 991 1000 204 218 779
1 581 1 289 1 217 1 557 1 558
331 86 402 406 23 600 42 389 165 354
832 256 293 287 104 945 90 881 76 81
2 669 767 665 12 172 31 861 46 43
781 677 365 84 58 740 259 737 455 558
392 251 493 642 361 66 236 297 651 154
1 314 1 497 1 195 1 156 172 362"

CodeJam.Tide.shortest H F C

CodeJam.Tide.solve "tide-small.in"
CodeJam.Tide.solve "tide-large.in"


CodeJam.Tide.test "200 1 2
          250 233
          180 100" 11.7 

    test "100 3 3
        500 500 500
        500 500 600
        500 140 1000
        10 10 10
        10 10 490
        10 10 10" 3.0

    test "100 3 3
        500 100 500
        100 100 500
        500 500 500
        10 10 10
        10 10 10
        10 10 10" 18.0

    test "100 2 2
        1000 1000
        1000 1000
        100 900
        900 100" 0.0



