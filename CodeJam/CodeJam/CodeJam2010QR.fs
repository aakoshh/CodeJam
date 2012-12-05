namespace CodeJam
open CodeJam.Utils


// http://code.google.com/codejam/contest/433101/dashboard#s=p0
module SnapperChain = 
    // open Utils
    // snapping is binary counting
    // 0000 -> 1000 -> 0100 -> 1100 -> 0010 -> 1010 -> 0110 -> ...
    // so after K snaps the state will be the K binary number
    let toBinary (i: int) = 
        System.Convert.ToString(i, 2)
    // the n-th one is on if all before it are on
    let snap n k = 
        let s = k |> toBinary |> List.ofSeq |> List.rev
        if s |> List.length < n then 
            "OFF" // did not count high enough
        else // check if all is "1"
            if s |> Seq.take n |> Seq.forall ((=) '1') then "ON" else "OFF"            
            
    assertEqual (snap 1 0) "OFF" "snap"
    assertEqual (snap 1 1) "ON" "snap"
    assertEqual (snap 4 0) "OFF" "snap"
    assertEqual (snap 4 47) "ON" "snap"

    let solve fn = 
        solveFile fn (fun line ->
            let [|n;k|] = line.Split([|' '|]) |> Array.map int
            snap n k)
    
    //solve "snapper-chain-sample.in"
    //solve "snapper-chain-small-practice.in"
    //solve "snapper-chain-large-practice.in" 
    


// http://code.google.com/codejam/contest/433101/dashboard#s=p1&a=0
module FairWarning = 
    // open Utils
    // greatest common divisor
    let rec gcd a b = 
        if b = 0I then a else gcd b (a%b)
    // at the time of the optimum all ti mod opt is equal
    let optimum ts = 
        // if all of them are congruent then if I subtract the minimum they will all be divisible by the optimum
        let tmin = ts |> Array.min
        // the optimum is the greatest common divisor of the numbers minus the min
        ts |> Array.map (fun t -> t-tmin) |> Array.sort |> Array.rev |> Array.reduce gcd
    
    // how much time left
    let apocalypse ts = 
        let opt = optimum ts
        // the minimum distance is the time till the first value is divisble by t
        match ts.[0] % opt with
        | m when m = 0I -> 0I // right now
        | m -> opt - m // time left

    assertEqual (apocalypse [|26000I;11000I;6000I|]) 4000I "optimum-anniversary"            
    assertEqual (apocalypse [|1I;10I;11I|]) 0I "optimum-anniversary"   
    assertEqual (apocalypse [|800000000000000000001I;900000000000000000001I|]) 99999999999999999999I "optimum-anniversary" 
    
    let solve fn = 
         solveFile fn (fun line ->
            line.Split([|' '|]).[1..] 
            |> Array.map (fun t -> System.Numerics.BigInteger.Parse(t))
            |> apocalypse |> string )

    //solve "fair-warning-sample.in"
    //solve "fair-warning-small-practice.in"
    //solve "fair-warning-large-practice.in"
                

// http://code.google.com/codejam/contest/433101/dashboard#s=p2&a=2
module ThemePark = 
    // open Utils
    // how much money does the roller coaster make
    let roll r k gs = 
        // rolls left, earnings so far, passengers, i-th group
        let n = gs |> Array.length
        let tr = r
        let rec loop r e ps ifirst i = 
            if r = 0 then
                e // no more runs, checkout
            elif r < tr && ps = 0 && i = 0 then
                // we are at the beginning of the queue agan with an empty train, we can just repeat
                tr/(tr-r) * e  // there might be something left
            else // try to fit in one more group
                let g = gs.[i]
                if ps + g > k then // cannot add more people
                    loop (r-1) (e+ps) 0 i i // start a run
                else // add the group
                    let i = (i+1) % n
                    if i = ifirst then // less groups then place on the train (empty queue)                    
                        loop (r-1) (e+ps+g) 0 i i               
                    else // try to add the next group too
                        loop r e (ps+g) ifirst i
        loop r 0 0 0 0

    assertEqual (roll 4 6 [|1;4;2;1|]) 21 "roll"
    assertEqual (roll 100 10 [|1|]) 100 "roll"
    assertEqual (roll 5 5 [|2;4;2;3;4;2;1;2;1;3|]) 20 "roll"

    roll 100000000 1000000000 (Array.create 1000 10000000)

    let solve fn = 
        solveFile2 fn (fun lines ->
            let [|l1;l2|] = lines.Split([|'\n'|])
            let [|r;k;_|] = l1.Split([|' '|]) |> Array.map int
            let gs = l2.Split([|' '|]) |> Array.map int
            roll r k gs |> string)

    //solve "theme-park-sample.in"
    //solve "theme-park-small-practice.in"
    //solve "theme-park-large-practice.in"

