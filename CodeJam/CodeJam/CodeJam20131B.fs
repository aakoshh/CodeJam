﻿namespace CodeJam
open CodeJam.Utils
open System

// https://code.google.com/codejam/contest/2434486/dashboard#s=p0
module Osmos = 

    let feed size target =         
        let rec loop steps size = 
            if size = 1 then
                None
            elif size > target then
                Some(size, steps)
            else
                loop (steps+1) (size + size-1)
        loop 0 size
    
    /// Given the initial mote A, calculate how many corrections are needed to absorb the list of motes.
    let moves a motes = 
        let rec loop corr size motes n = 
            match motes with
            | [] -> 
                corr
            | h::t when h < size -> 
                loop corr (size + h) t (n-1)
            | h::t -> // h >= size
                // we can either delete, or add the maximum edible mote until we can consume the next.
                // since motes are in order, if we delete one, we can delete all.
                let deletion = corr + n
                match feed size h with
                | Some(size, steps) when (corr+steps) < deletion ->
                    let rest = loop (corr+steps) size motes n
                    min deletion rest
                | _ -> 
                    deletion
        // consume the smallest first
        loop 0 a (motes |> List.sort) (motes |> List.length)


    let test() = 
        assertEqual (moves 10 [9;13;19]) 0 "moves"
        assertEqual (moves 10 [9;20;25;100]) 2 "moves"
        assertEqual (moves  2 [2;1]) 0 "moves"
        assertEqual (moves  2 [2;1;1;6]) 1 "moves"
        assertEqual (moves  1 [1;1;1;1]) 4 "moves"
        assertEqual (moves  3 [11;20;60;22;100]) 3 "moves"

    let solve fn = 
        solveFile2 fn <| fun lines ->
            let rows = lines |> splitLines
            let todata = splitSpaces >> Array.map int
            let [|A; _|] = rows.[0] |> todata
            let motes = rows.[1] |> todata |> List.ofArray
            moves A motes |> string

    // CodeJam.Osmos.solve "osmos-sample-practice.in"

    

// https://code.google.com/codejam/contest/2434486/dashboard#s=p1
module FallingDiamonds = 
    
    // diamonds will form a perfect heap of some height, t
    // then randomly cover its next layer until they run out.
    // at that point the point of interest is either within the heap, 
    // or out of the next layer, or there is a binomial distribution
    // that needs to be calculated to get the probability.

    /// Enumerate diamond heap sizes and free places in the outer layer.
    let heaps = 
        let rec loop ((size, outer, height) as hp) = seq {            
            yield hp
            // outer will always be an odd number, so both sides will have ceil(outer/2) sides to be covered,
            // plus the one that goes in the ground, plus a new top 
            let size, outer, height = 
                size + outer, 
                (outer/2+1 + 1) * 2 + 1, 
                height + 2 // center of the top
            yield! loop (size, outer, height)
        }
        loop (1,5,0)

    /// Combinations
    let combinations n k = 
        let n = seq {(n-k+1) .. n} |> Seq.fold (*) 1
        let d = seq {1 .. k} |> Seq.fold (*) 1
        n / d

    /// Calculate the probability of having a diamond out of n centered at (x,y)
    let prob n (x,y) = 
        // find the last perfect heap that is not larger than n
        let (size, outer, height) = 
            heaps 
            |> Seq.takeWhile (fun (size, _, _) -> size <= n) 
            |> Seq.last

        // over the outer layer? 
        if abs(x) + y > 0 + (height+2) then 
            0.0 
        // within the perfect heap?
        elif abs(x) + y <= 0 + height then 
            1.0 
        // in the outer layer, y can be from 0 to height+2
        else
            // number of diamonds filling the outer layer
            let sliders = n - size

            // enough sliders to fill the opposite side and still reach y?
            if sliders >= (outer / 2) + (y+1) then
                1.0 
            // not enough sliders to reach y at all?
            elif sliders < (y+1) then
                0.0 
            // asking for the top?
            elif y = height + 2 then
                0.0 // would be a perfect heap and we are not there
            // may or may not reach, depending on how much falls on the it's side
            else
                // how many ways can we fail to reach (y+1)
                let pfail = [0 .. y] // number of diamonds going towards y
                            |> List.sumBy (fun r -> 
                                let c = combinations sliders r
                                (float c) * 0.5**(float r) * 0.5**(float(sliders-r)))
                1.0 - pfail

    let test() = 
        let check n (x,y) (p: float) = 
            let p' = prob n (x,y)
            assertEqual (Math.Round(p',2)) (Math.Round(p,2)) (sprintf "prob %d (%d,%d)" n x y)

        check 1 (0,0) 1.0
        check 1 (0,1) 0.0
        check 1 (0,2) 0.0
        check 1 (1,0) 0.0
        check 2 (0,1) 0.0
        check 2 (1,0) 0.5
        check 3 (1,0) 0.75  
        check 3 (0,0) 1.0 
        check 3 (2,0) 0.75 
        check 3 (1,1) 0.25
        check 4 (1,1) 0.5
        check 4 (0,2) 0.0


    let solve fn = 
        solveFile fn <| fun line ->
            let [|N; X; Y|] = line |> (splitSpaces >> Array.map int)            
            prob N (X,Y) |> sprintf "%.6f"


    // CodeJam.FallingDiamonds.solve "fallingdiamonds-sample-practice.in"
    // CodeJam.FallingDiamonds.solve "fallingdiamonds-small-practice.in"
    // CodeJam.FallingDiamonds.solve "fallingdiamonds-large-practice.in"


// https://code.google.com/codejam/contest/2434486/dashboard#s=p2
module GarbledEmail = 

    open System.IO

    let readDict() = 
        let fn = "garbled_email_dictionary.txt" |> toPath
        File.ReadAllLines(fn) |> List.ofArray

    module Exhaustive = 
        /// Try to match a string with a word with at least 5 between errors.
        let rec matchWord s w i e = 
            // i and e are the indices of the last character matched and the last error encountered
            let pos = i + 1
            match s, w with
            | rest, [] -> 
                Some(rest, i, e) // matched the whole word
            | [], _ -> 
                None // ran out of strings before word completed
            | cs::rs, cw::rw when cs = cw ->
                matchWord rs rw pos e // matched character
            | _::rs, _::rw when pos - e >= 5 || e = 0 ->
                matchWord rs rw pos pos // allowable mismatch
            | _ -> 
                None // to early mismatch


        /// Enumerate all the possible matches using a dictionary.
        let matchWords d s = 
            // accumulate the matched words going through the dictionary
            let rec loop ws s i e = seq {
                for w in d do
                    match matchWord s w i e with
                    | Some([], _, _) -> 
                        yield (w::ws) |> List.rev |> List.collect id
                    | Some(s', i', e') -> 
                        yield! loop (w::ws) s' i' e'
                    | None -> 
                        ()
            }
            loop [] s 0 0
    
        let toChars s = s |> List.ofSeq

        /// Count the number of characters two lists differ by
        let dist s1 s2 = 
            List.zip s1 s2 |> List.fold (fun d (a,b) -> if a = b then d else d+1) 0

        /// Find the best match
        let minDist d s = 
            let replacements = dist s
            matchWords d s
            |> Seq.map replacements
            |> Seq.min

        let test() = 
        
            let d = ["code"; "jam"] |> List.map toChars |> Set.ofList

            assertEqual (matchWord (toChars "dodejbm") (toChars "code") 0 0) (Some (['j'; 'b'; 'm'], 4, 1)) "matchWord"
            assertEqual (matchWord (toChars "dam") (toChars "jam") 4 1) None "matchWord"

            let orig = ['c'; 'o'; 'd'; 'e'; 'j'; 'a'; 'm']
            assertEqual (matchWords d (toChars "dodejbm") |> List.ofSeq) [orig] "matchWords"
            assertEqual (matchWords d (toChars "zodejan") |> List.ofSeq) [orig] "matchWords"
            assertEqual (matchWords d (toChars "cidejab") |> List.ofSeq) [orig] "matchWords"
            assertEqual (matchWords d (toChars "kodezam") |> List.ofSeq) [] "matchWords"

    
    module Dynamic = 
        
        type MatchResult = 
            | Perfect
            | Changes of int * int * int // how many, first, last
            | Failure

        /// Match a word array against the part of another array between i and j
        let matchWord (s: 'a array) (w: 'a array) i j = 
            let n = w |> Array.length
            if n <> j-i+1 then
                Failure
            else
                let rec loop k sofar = 
                    if k = n then
                        sofar
                    elif s.[i+k] = w.[k] then
                        loop (k+1) sofar
                    else
                        match sofar with 
                        | Perfect -> 
                            loop (k+1) (Changes(1, k, k))
                        | Changes(n, f, l) when k - l >= 5 -> 
                            loop (k+1) (Changes(n+1, f, k))
                        | _ -> 
                            Failure  
                match loop 0 Perfect with
                | Changes(c, f, l) -> Changes(c, i+f, i+l)
                | result -> result


        let matchWords (d: 'a array list) (s: 'a array)  = 
            let n = s |> Array.length
            /// Find the least amount of substitutions to get s up to j with the last error maximum at l
            let rec bestMatch = memoize <| fun (j,l) ->
                if j < 0 then
                    Perfect, []
                else
                    seq {
                        // try to match every word at the end of the string
                        for w in d do
                            let i = j - (w.Length-1)
                            if i >= 0 then // word not longer than prefix
                                match matchWord s w i j with
                                | Perfect -> 
                                    let mr, ws = bestMatch(i-1,i-1) 
                                    yield mr, w :: ws
                                | Changes(cw, first, last) as chng when last <= l ->
                                    // have to check that the prefix can be matched without violating this
                                    match bestMatch(i-1,first-5) with
                                    | Perfect, ws -> 
                                        yield chng, w :: ws
                                    | Changes(cp, fp, lp), ws -> 
                                        yield Changes(cp+cw, fp, last), w :: ws  
                                    | Failure, _ as fail -> 
                                        yield fail
                                | _ -> yield Failure, []  
                            else 
                                yield Failure, []                              
                    }  
                    // find the best match in terms of least amount of replacements
                    |> Seq.minBy (function 
                        | Perfect, _ -> 0
                        | Changes(c, _, _), _ -> c
                        | Failure, _ -> j)
            let merge ws = 
                ws |> List.rev |> List.map List.ofArray |> List.collect id |> Array.ofList
            // find the best match and reverse the words
            match bestMatch(n-1,n-1) with
            | Failure, _ -> None
            | Perfect, ws -> Some(0, merge ws)            
            | Changes(c,_,_), ws -> Some(c, merge ws) 
                     

        let test() = 
        
            let d = ["code"; "jam"] |> List.map Array.ofSeq

            assertEqual (matchWord (Array.ofSeq "dodejbm") (Array.ofSeq "jam") 4 6) (Changes (1,5,5)) "matchWord"
            assertEqual (matchWord (Array.ofSeq "codejob") (Array.ofSeq "jam") 4 6) Failure "matchWord"

            assertEqual (matchWords d (Array.ofSeq "codejam")) (Some (0, Array.ofSeq "codejam")) "matchWords"
            assertEqual (matchWords d (Array.ofSeq "dodejbm")) (Some (2, Array.ofSeq "codejam")) "matchWords"
            assertEqual (matchWords d (Array.ofSeq "kodezam")) None "matchWords"            


    open Dynamic

    let solve fn = 
        let d = readDict() |> List.map Array.ofSeq
        solveFile fn <| fun line ->
            let S = line |> Array.ofSeq          
            match matchWords d S with
            | Some(d, _) -> string d
            | None -> string S.Length // shouldn't happen


