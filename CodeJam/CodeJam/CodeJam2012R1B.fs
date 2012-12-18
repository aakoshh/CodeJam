namespace CodeJam
open CodeJam.Utils

// http://code.google.com/codejam/contest/1836486/dashboard

module SafetyInNumbers =     

    /// given an array of points, calculate the minimum support each point needs not to be eliminated
    let thresholds points = 
        let points = points |> Array.map float
        let total  = points |> Array.sum       
        // global mean is where the water would be max, anyone above that is safe    
        let meanG  = total * 2.0 / (float points.Length)
        // residual mean is the water level where there is no land
        let resid  = points |> Array.filter ((>) meanG)
        let nR     = float resid.Length
        let meanR  = ((resid |> Array.sum) + total) / nR
        // you have to be the average, that way you won't be the last
        points
        |> Array.map (fun x -> 
            max 0.0 ((meanR-x)/total))          
        |> Array.map ((*) 100.0)

    let test = thresholds >> Array.map int

    assertEqual (test [|20;10|]) [|33; 66|] "thresholds"
    assertEqual (test [|10;0|]) [|0; 100|] "thresholds"
    assertEqual (test [|25;25;25;25|]) [|25; 25; 25; 25|] "thresholds"
    assertEqual (test [|24;30;21|]) [|34; 26; 38|] "thresholds"

    open System 

    let solve fn = 
        solveFile fn (fun line ->
            let points = line |> splitSpaces |> Array.map int
            points.[1..] |> thresholds |> Array.map (sprintf "%.6f") |> joinSpaces)

    // solve "safety-in-numbers-sample.in"
    // solve "safety-in-numbers-small.in"



module Tide = 
    
    // shortest path (in time) from NW to SE corner with Dijkstra
    // H: water height, F: floor height, C: ceiling height
    let shortest (H: float) (F: float[,]) (C: float[,]) = 
        // boundaries
        let N, M = F |> Array2D.length1, F |> Array2D.length2
        let start, target = (0,0), (N-1,M-1)
        let speed = 10.0 // water drops 10cm per second

        // shortest distance matrix
        let S = F |> Array2D.map (fun _ -> None)
         
        // enumerate the the time required to reach the neighbouring cells from (i,j) at time t
        let moves t (i,j) = seq {
            let w = max 0.0 (H - t * speed) // water went down 10 cm per second
            let p = max w F.[i,j] // where are we
            let cells = [i-1,j; i,j+1; i+1,j; i,j-1] 
                        |> List.filter (fun (x,y) -> 
                            x >= 0 && x < N && y >= 0 && y < M)
            for (x,y) in cells do
                let md = 50.0 // minimum height
                // check if we can move
                if F.[x,y] + md <= C.[x,y] + 0.01 && // target has enough height
                   F.[i,j] + md <= C.[x,y] + 0.01 &&
                   F.[x,y] + md <= C.[i,j] + 0.01 then
                    if p + md <= C.[x,y] + 0.01 then // we can slide without bumping the head
                        // see if we are still at zero time, in which case we can just move without any cost
                        if t = 0.0 then
                            yield t, (x,y)
                        else 
                            // time to move is 1sec if we have at least 20cm of water othewise 10sec
                            let dt = if w >= F.[i,j] + 20.0 then 1.0 else 10.0
                            yield t + dt, (x,y)   
                    else // we have to wait because the next room has too much water in it
                        let pc = max w F.[x,y] // now
                        let pt = C.[x,y] - md // target
                        let dt = (pc - pt) / speed // time to wait
                        let t' = t + dt
                        let w' = max 0.0 (H - (t' * speed))
                        let dt' = if w' >= F.[i,j] + 20.0 then 1.0 else 10.0
                        yield t' + dt', (x,y)            
        }

        // loop to implement Dijkstra
        let rec loop Q = 
            // get the closest tile with it's water level
            let (t, (i,j)) as s = Q |> Set.minElement
            let Q = Q |> Set.remove s
            // see where we are
            if (i,j) = target then
                S.[i,j] <- Some(t)
                S
            elif S.[i,j].IsSome then
                loop Q
            else // first time we entered this square
                S.[i,j] <- Some(t) 
                let next = moves t (i,j) |> List.ofSeq
                //printfn "%.1f: %A (%.0f) -> %A" t (i,j) p next
                let Q = Set.union Q (next |> Set.ofSeq) 
                loop Q

        // start from the upper corner, where we are
        let Q = Set.singleton (0.0, start)
        loop Q
                

    let toCase lines = 
        // tests
        let mat = lines |> splitLines |> Array.map (splitSpaces >> Array.map int)
        let [|H; N; M|] = mat.[0]
        let C = array2D mat.[1..N] |> Array2D.map float
        let F = array2D mat.[(N+1)..(N+N)] |> Array2D.map float
        float H, F, C


    let solveCase lines = 
        let H,F,C = lines |> toCase
        let S = shortest H F C
        let N = S |> Array2D.length1
        let M = S |> Array2D.length2
        S.[N-1, M-1] |> Option.get

    let test lines et = 
        let t = (lines |> solveCase) * 10.0 |> int
        assertEqual t (int(et*10.0)) "tide"


    test "200 1 2
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


    let solve fn = 
        let solver = solveFileBy (caseByDynWithHeader (fun pline -> // the header line tells that 2*N matrix follow 
                                    2 * (int (pline.Split([|' '|]).[1]))))
        solver fn (solveCase >> (sprintf "%.1f"))


    // CodeJam.Tide.solve "tide-small.in"
    // CodeJam.Tide.solve "tide-large.in"



module EqualSums = 

    /// signal the first sum that is found in two different ways
    exception DoublePath of (bigint*bigint) * Map<bigint,bigint>

    /// add a number to each element of a set and remember the parent
    let add (i: bigint) P = 
        // for every sum in the current mapping
        let ns = P |> Map.toSeq |> Seq.map (fun (k,v) -> k) |> List.ofSeq |> List.sort
        let rec loop ks m = 
            match ks with 
            | [] -> m
            | h::t -> 
                let s = h + i
                if Map.containsKey s m then
                    raise <| DoublePath((s,h), m)
                else
                    let m' = m |> Map.add s h
                    loop t m'
        loop ns P

    /// trace back the parents of a number till zero
    let trace (i: bigint) P = 
        let rec loop i ps = 
            if i = 0I then
                ps
            else
                // what's the parent of i
                let p = P |> Map.find i
                let d = i - p
                loop p (d::ps)
        loop i []

    /// add numbers until we find one that has a second parent
    let partition ns = 
        let rec loop ns P = 
            match ns with
            | [] -> 
                None // it was impossible to find two subsets
            | h::t -> 
                try 
                    let P' = add h P 
                    loop t P'
                with
                | DoublePath( (s,p), M ) -> 
                    let p1 = trace s M
                    let p2 = (trace p M) @ [h]
                    Some(p1,p2)
        let P0 = [0I,0I] |> Map.ofSeq
        loop ns P0

    
    let P0 = [0I,0I] |> Map.ofSeq

    let check (Some(l1: bigint list, l2: bigint list)) =
            let s1 = l1 |> List.sum
            let s2 = l2 |> List.sum
            assertEqual s1 s2 "sums"
            let S1 = l1 |> Set.ofList
            let S2 = l2 |> Set.ofList
            assertEqual (Set.intersect S1 S2) Set.empty "distinct"
       
    assertEqual (P0 |> add 1I |> add 2I |> add 5I) ([0I,0I; 1I,0I; 2I,0I; 3I,1I; 5I,0I; 6I,1I; 7I,2I; 8I,3I] |> Map.ofList) "addparent"
    assertEqual (P0 |> add 1I |> add 2I |> add 5I |> trace 7I) [2I;5I] "trace"

    let test() = 
        let t1 = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20] |> List.map (fun x -> bigint x) |> partition
        assertEqual t1 (Some ([1I; 2I], [3I])) "partition"
        check t1

        let t2 = [120;266;858;1243;1657;1771;2328;2490;2665;2894;3117;4210;4454;4943;5690;6170;7048;7125;9512;960] |> List.map (fun x -> bigint x) |> partition
        check t2
        assertEqual t2 (Some ([1243I; 1771I], [120I; 2894I])) "partition"  
        
    test()      


    let solve fn = 
        let toLine xs = 
            xs |> List.map string |> Array.ofList |> joinSpaces

        solveFile fn (fun line -> 
            let ns = line |> splitSpaces |> List.ofArray |> List.map (fun x -> bigint.Parse(x))
            match partition (ns |> List.tail) with
            | Some(l1,l2) -> 
                sprintf "\n%s\n%s" (l1 |> toLine) (l2 |> toLine)
            | None -> 
                "Impossible")

    // CodeJam.EqualSums.solve "equal-sums-sample.in"



