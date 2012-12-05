namespace CodeJam
open CodeJam.Utils

// http://code.google.com/codejam/contest/1645485/dashboard#s=p0
module PasswordProblem = 
    
    // find out if the minimum expected keystrokes for strategies
    let averageKeyStrokes typed total probs = 
        // prepare cumulative probabilities
        let n = probs |> Array.length
        let cps = Array.create n 0.0
        cps.[0] <- probs.[0]
        for i in 1 .. n-1 do
            cps.[i] <- cps.[i-1] * probs.[i]
        // expected number of keystrokes if keeping the first i
        let expected i =   
            if i = 0 then
                0.0, float (1 + total + 1)
            else             
                // p(correct) * rest + p(incorrect) * (rest + again)
                let p = cps.[i-1]
                let m = (float (max 0 (typed - i))) // to delete the good
                        + p * (float (total - i + 1)) // if the remaining is correct
                        + (1.0 - p) * (float (total - i + 1 + total + 1)) // if it's not                
                1.0 - p, m
        // strategies
        let rec loop i = seq {
            let pfail, m = expected i
            yield m
            if pfail > 0.0 && i > 1 then
                yield! loop (i-1)
        }
        seq { 
            yield (expected 0) |> snd // one keystrike to get rid of everything
            yield! loop typed // try holding on to typed input until we find a 100% situation
        } |> Seq.min

    assertEqual (averageKeyStrokes 2 5 [|0.6; 0.6|]) 7.0 "password"
    assertEqual (averageKeyStrokes 1 20 [|1.0|]) 20.0 "password"
    assertEqual (averageKeyStrokes 3 4 [|1.0; 0.9; 0.1|]) 4.5 "password"

    let solve fn = 
        // two lines per case
        solveFile2 fn (fun lines ->
            let [|ab;ps|] = lines |> splitLines
            let [|a;b|] = ab |> splitSpaces |> Array.map int
            let ps = ps |> splitSpaces |> Array.map float            
            averageKeyStrokes a b ps |> sprintf "%.6f")

    // solve "password-problem-sample.in"
    // solve "password-problem-small-practice.in"
    // solve "password-problem-large-practice.in"


// http://code.google.com/codejam/contest/1645485/dashboard#s=p1
module KingdomRush = 
    
    // find the minimum number of plays to complete
    let minPlays (levels: int[] list) = 
        // maintain a set of the cheapest 2 star levels and cheapest 1 star levels
        // see which 2 star level we can complete. if none, see which 1 star level
        let n = levels |> List.length
        // star requirements
        let req = levels |> Seq.mapi (fun i stars -> (i,stars)) |> Map.ofSeq
        let cheapest star = 
            levels |> Seq.mapi (fun i stars -> stars.[star],i) |> Set.ofSeq
        let rec loop stars plays togo completed cheapest1 cheapest2 = 
            //printfn "%d stars after %d plays with %d levels to go. (%A, %A, %A)" stars plays togo completed cheapest1 cheapest2 
            if togo = 0 then
                Some(plays)
            else
                // see which 2 stars we can complete
                match cheapest2 |> Set.minElement with
                | (minStars,lvl) when stars >= minStars -> // we can complete a level
                    // how many stars do we gain?
                    let stars = stars + if completed |> Set.contains lvl then 1 else 2
                    let completed = completed |> Set.add lvl
                    let cheapest1 = cheapest1 |> Set.remove (req.[lvl].[0], lvl)
                    let cheapest2 = cheapest2 |> Set.remove (req.[lvl].[1], lvl)
                    loop stars (plays+1) (togo-1) completed cheapest1 cheapest2
                | _ -> // cannot complete any. try the 1 stars
                    if cheapest1.Count > 0 then
                        match cheapest1 |> Set.minElement with
                        | (minStars,lvl) when stars >= minStars -> // we can complete a level
                            let stars = stars + 1
                            let completed = completed |> Set.add lvl
                            let cheapest1 = cheapest1 |> Set.remove (req.[lvl].[0], lvl)
                            loop stars (plays+1) togo completed cheapest1 cheapest2
                        | _ -> // cannot complete that either
                            None
                    else // we have 2 stars to go but no 1 star left
                        None
        match loop 0 0 n Set.empty (cheapest 0) (cheapest 1) with
        | Some(plays) -> plays |> string
        | None -> "Too Bad"


    assertEqual (minPlays [[|0;1|];[|0;2|]]) "3" "KingdomRush"
    assertEqual (minPlays [[|2;2|];[|0;0|];[|4;4|]]) "3" "KingdomRush"
    assertEqual (minPlays [[|1;1|]]) "Too Bad" "KingdomRush"
    assertEqual (minPlays [[|0;5|];[|0;1|];[|1;1|];[|4;7|];[|5;6|]]) "6" "KingdomRush"
    assertEqual (minPlays [ [|6;9|];[|14;18|];[|3;14|];[|0;0|];[|2;9|];[|7;13|];[|5;7|];[|4;5|];[|7;14|] ]) "Too Bad" "KingdomRush"


    let solve fn = 
        solveFileN fn (fun lines ->
            let levels = lines |> splitLines |> Seq.ofArray |> Seq.map (splitSpaces >> Array.map int) |> List.ofSeq
            levels |> minPlays)
    
    // solve "kingdom-rush-sample.in"
    // solve "kingdom-rush-small-practice.in"
    // solve "kingdom-rush-large-practice.in"


