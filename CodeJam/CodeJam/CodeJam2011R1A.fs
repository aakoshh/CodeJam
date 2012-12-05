namespace CodeJam
open CodeJam.Utils

// http://code.google.com/codejam/contest/1145485/dashboard
module FreeCell = 
    // open Utils
    open System
    open System.IO    

    let isInteger (d: float) = 
        Math.Floor(d) = d

    let toPcnt p = (float p) / 100.0

    // find all the possible values of 0 < D <= N so that D * P(D) is an integer value
    let findDaysPlays n pd = 
        let pd = pd |> toPcnt
        // the first solution is enough, the doubles, triples won't make a difference in the condition of G
        let rec loop d = 
            if d > n then 
                None
            elif (float d) * pd |> isInteger then
                Some(d)
            else
                loop (d+1L)
        loop 1L

    // find a possible solution to the number of total plays so that
    // (1-P(D)) * D <= (1-P(G)) * G and P(D) * D <= P(G) * G and P(G) * G is integer
    // but when can we stop?
    let findGlobalPlays pd pg d =         
        let [|pd; pg; npd; npg|] = [|pd; pg; 100L-pd; 100L-pg|] |> Array.map toPcnt
        let wd = (float d) * pd
        let nwd = (float d) * npd
        // find the first solution
        let rec loop g = 
            if wd > 0.0 && pg = 0.0 || nwd > 0.0 && npg = 0.0 then // no need to iterate
                None
            else
                let G = float g            
                if G * pg |> isInteger && wd <= G * pg && nwd <= G * npg then
                    Some(g)
                else
                    loop (g+1L)
        loop d

    // find out if a row is a possible play result
    let isPossible n pd pg = 
        match findDaysPlays n pd with
        | Some(d) -> 
            match findGlobalPlays pd pg d with
            | Some(g) -> true
            | None -> false
        | None -> false

    let solve fn =         
        solveFile fn (fun line ->
            let [|n;pd;pg|] = line.Split( [|' '|] ) |> Array.map (fun s -> System.Int64.Parse(s))
            if isPossible n pd pg then "Possible" else "Broken" )


    assertEqual (isPossible 1L 100L 50L) true "isPossible"
    assertEqual (isPossible 10L 10L 100L) false "isPossible"
    assertEqual (isPossible 9L 80L 56L) true "isPossible"    

    //solve "freecell.txt"
    //solve "freecell-small-practice.in"
    //solve "freecell-large-practice.in"

