namespace CodeJam
open CodeJam.Utils
open System



// https://code.google.com/codejam/contest/2418487/dashboard#s=p0
module Bullseye = 

    // paint needed to cover a ring starting at r
    let paint (r: bigint) = 
        // ((r+1I) * (r+1I) - r * r) = r**2 + 2*r + 1 - r**2 
        // paint(n+1) - paint(n) = 2n+2+1 - 2n-1 = 2
        2I * r + 1I

    let count (r: bigint) (t: bigint) = 
        let rec loop n t = 
            let p = paint (r + 2I*n)
            if t < p then n else loop (n+1I) (t-p)
        loop 0I t

    let calculate (r: bigint) (t: bigint) = 
        // every new ring needs +4 than the one before
        // S = (a1 + a1+(n-1)*4)/2 * n = 2*n**2 + (a1-2)*n
        let a1 = paint r 
        let d = a1*a1 - 4I*a1 + 4I + 8I*t
        // cannot use double due to rounding errors
        let n = (2I - a1 + Numerics.sqrti(d)) / 4I
        n
        
    let solve fn = 
        solveFile fn (fun line ->
            let [|r;t|] = line |> splitSpaces |> Array.map (fun x -> bigint.Parse(x))
            calculate r t |> string)

    // CodeJam.Bullseye.solve "bullseye-sample-practice.in"
    // CodeJam.Bullseye.solve "bullseye-small-practice.in"
    // CodeJam.Bullseye.solve "bullseye-large-practice.in"

