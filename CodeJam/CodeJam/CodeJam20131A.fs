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


// https://code.google.com/codejam/contest/2418487/dashboard#s=p1
module ManageYourEnergy = 

    // maximum gain
    let gain E R V = 
        let maxi = (V |> Array.length) - 1
        // calculate maximum gain from task i with e energy to spend on them
        let rec maxgain = memoize <| fun (i, e) ->  
            if i = maxi then
                V.[i] * e, [e]
            else                     
                // find the first point where we lose more from the rest than we gain from this
                let rec loop j (bg, bj) = 
                    if j > e then 
                        bg, bj
                    else
                        let g = V.[i] * j
                        let e' = min E (e - j + R)
                        let gr, jr = maxgain (i+1, e')
                        if g + gr < bg then
                            bg, bj
                        else
                            loop (j+1L) (g+gr, j::jr)

                // maximum from the rest if we spend 0 here
                let e' = min E (e + R)
                let mg, mj = maxgain (i+1, e')
                loop 1L (mg, 0L::mj)

        let best = maxgain (0, E)
        // printfn "%A" (snd best)
        fst best


    let solve fn = 
        solveFile2 fn (fun lines ->
            let [|l1;l2|] = lines |> splitLines
            let [|E;R;_|] = l1 |> splitSpaces |> Array.map int64
            let V = l2 |> splitSpaces |> Array.map int64
            gain E R V |> string)


    // CodeJam.ManageYourEnergy.solve "manageyourenergy-sample.in"
    // CodeJam.ManageYourEnergy.solve "manageyourenergy-small-practice.in"


    // gain 9174849L 3990053L [|9029186L;6768994L|]
    // gain 3646321L 205998L [|9123315L;8377335L;3886394L|]