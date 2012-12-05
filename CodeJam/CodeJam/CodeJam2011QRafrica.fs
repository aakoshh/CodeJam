namespace CodeJam
open CodeJam.Utils

// http://code.google.com/codejam/contest/837485/dashboard
module ClosingTheLoop = 
    // open Utils

    // given a list of segments, find the longest loop
    let close segments = 
        // the segments will need to be paired up
        let blue,red = segments |> List.sortBy (fun (l,c) -> -l)
                                |> List.partition (fun (l,c) -> c = 'B')
        // the longest loop consists of segments up to the shorter list of the two
        let minl = min (blue |> List.length) (red |> List.length)
        // length of the first n
        let len s = 
            s |> Seq.take minl |> Seq.map fst |> Seq.sum
        (len blue) + (len red) - minl * 2 // each segment will lose 1 cm to knots, there are 2*minl segments

    
    assertEqual (close [5,'B']) 0 "close-loop"
    assertEqual (close [6,'R'; 1,'B'; 7,'R'; 3,'B']) 13 "close-loop"
    assertEqual (close [5,'B'; 4,'R'; 3,'R'; 2,'R'; 5,'R'; 4,'R'; 3,'R']) 8 "close-loop"
    assertEqual (close [20,'B'; 20,'R']) 38 "close-loop"

    // solve a case
    let linesToSegments (lines: string) =
        (lines |> splitLines).[1]  // first line is the number of segments
            |> splitSpaces 
            |> Array.map (fun lc -> 
                (lc.Substring(0,lc.Length-1) |> int, lc.[lc.Length-1])) // each segment becomes a tuple of (length,color)        
            |> List.ofArray

    let solve fn = 
        solveFile2 fn (linesToSegments >> close >> string)

    //solve "closing-the-loop-sample.in"
    //solve "closing-the-loop-small-practice.in"
    //solve "closing-the-loop-large-practice.in"



// http://code.google.com/codejam/contest/837485/dashboard#s=p1
module InvestingTheMarket = 
    // open Utils

    // given a list of segments, find the longest loop
    let invest (money, prices) = 
        // choose the highest investment
        let choices = seq {
            let n = prices |> Array.length
            for i in 0 .. n-2 do
                for j in i+1 .. n-1 do
                    // yield profit, initial investment, month indices
                    let bought = money / prices.[i] // maybe zero
                    yield ( (prices.[j] - prices.[i]) * bought, // profit
                            prices.[i],  // price per unit
                            i+1, j+1 )
        }
        // choose the best
        let best = choices |> Seq.maxBy (fun (p,i,_,_) -> (p,-i))
        match best with
        | p,_,_,_ when p <= 0 -> "IMPOSSIBLE"
        | p,i,m1,m2 -> 
            sprintf "%d %d %d" m1 m2 p

    
    assertEqual (invest (100,[|1;2;3;4;5;6;7;8;9;10;11;12|])) "1 12 1100" "invest"
    assertEqual (invest (100,[|52;50;25;100;61;63;70;51;71;55;10;5|])) "3 4 300" "invest"
    assertEqual (invest (100,[|200;150;250;132;125;110;210;220;180;176;108;113|])) "IMPOSSIBLE" "invest"

    // solve a case
    let linesToInvestment (lines: string) =
        let lines  = (lines |> splitLines) // first the money, then the values
        let money  = lines.[0] |> int
        let prices = lines.[1] |> splitSpaces |> Array.map int
        (money,prices)

    let solve fn = 
        solveFile2 fn (linesToInvestment >> invest )

    //solve "investing-sample.in"
    //solve "investing-small-practice.in"
    //solve "investing-large-practice.in"


// http://code.google.com/codejam/contest/837485/dashboard#s=p2&a=1
module BuildingHouse = 
    // open Utils

    let build map =         
        // G and S can be used, the others are obstacles
        let isObstacle = function 
            | 'G' | 'S' -> false | _ -> true
        // find the largest rectangular area
        let rec loop (r,c) map =         
            let h,w = map |> Array2D.length1, map |> Array2D.length2
            // need to check for obstacles
            if isObstacle map.[r,c] then // we can try to look in four planes around the obstacle
                let planes = [0,0,h-1,c-1; 0,0,r-1,w-1; r+1,0,h-1,w-1; 0,c+1,h-1,w-1] 
                                |> List.filter (fun (rt,ct,rb,cb) -> 
                                       0 <= rt && rt < h && 0 <= ct && ct < w 
                                    && 0 <= rb && rb < h && 0 <= cb && cb < w )
                                |> List.map (fun (rt,ct,rb,cb) ->
                                    let arr = map.[rt..rb,ct..cb]
                                    loop (0,0) arr)
                match planes with 
                | [] -> 0 // probably was already a 1x1 cell
                | _ -> planes |> List.max
            elif r = h-1 && c = w-1 then
                h * w // completed square
            else // not an obstacle, look further 
                let (r,c) = if c+1 < w then (r,c+1) else (r+1,0) // next cell to check
                loop (r,c) map
        loop (0,0) map
        
    assertEqual (build (array2D [['G']])) 1 "building"
    assertEqual (build (array2D [['G';'S'];['S';'G']])) 4 "building"
    assertEqual (build (array2D [['G';'T'];['G';'G']])) 2 "building"
    assertEqual ("GGTGG
TGGGG
GSSGT
GGGGT
GWGGG
RGTRT
RTGWT
WTWGR" |> splitLines |> Array.map Array.ofSeq |> array2D |> build) 9 "building"

    let solve fn = 
        solveFileWxH fn (fun lines -> // string map
            lines 
            |> splitLines 
            |> Array.map Array.ofSeq
            |> array2D |> build |> string)

    //solve "building-house-sample.in"
    //solve "building-house-small-practice.in"
    //solve "building-house-large-practice.in"

    [1;2;2] |> List.filter((=) 0) |> List.max
