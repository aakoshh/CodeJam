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



    


            



