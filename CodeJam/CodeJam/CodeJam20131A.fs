namespace CodeJam
open CodeJam.Utils
open System



// https://code.google.com/codejam/contest/2418487/dashboard#s=p0
module Bullseye = 

    // paint needed to cover a ring starting at r
    let paint (r: bigint) = 
        ((r+1I) * (r+1I) - r * r) 

    let count (r: bigint) (t: bigint) = 
        let rec loop n t = 
            let p = paint (r + 2I*n)
            if t < p then n else loop (n+1I) (t-p)
        loop 0I t

    let solve fn = 
        solveFile fn (fun line ->
            let [|r;t|] = line |> splitSpaces |> Array.map (fun x -> bigint.Parse(x))
            count r t |> string)