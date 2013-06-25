namespace CodeJam
open CodeJam.Utils
open System


// https://code.google.com/codejam/contest/2434486/dashboard#s=p0
module Osmos = 

    let feed size target = 
        if size = 1 then 
            failwith "cannot grow from 1"
        let rec loop steps size = 
            if size > target then
                size, steps
            else
                loop (steps+1) (size + size-1)
        loop 0 size
    
    /// Given the initial mote A, calculate how many corrections are needed to absorb the list of motes.
    let moves a motes = 
        let rec loop corr size motes = 
            match motes with
            | [] -> 
                corr
            | h::t when h < size -> 
                loop corr (size + h) t
            | h::t -> // h >= size
                // we can either delete, or add the maximum edible mote until we can consume the next                
                let cd = loop (corr+1) size t
                if size > 1 then
                    let fed, steps = feed size h
                    let cf = loop (corr+steps) fed motes
                    min cd cf
                else
                    cd
        // consume the smallest first
        loop 0 a (motes |> List.sort)


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

    


