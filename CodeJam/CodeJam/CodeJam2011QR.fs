namespace CodeJam
open CodeJam.Utils


// http://code.google.com/codejam/contest/975485/dashboard#s=p0
module BotTrust = 
    // open Utils    
    // count the number of steps to finish the steps
    let walk buttons = 
        // separate the two colors
        let colorButtons color = buttons |> List.filter (fun (c,i) -> c = color) |> List.map (fun (c,i) -> i)
        let ob = colorButtons 'O' // orange
        let bb = colorButtons 'B' // blue
        // go towards the next point
        let approach bs p = 
            match bs with
            | i::rest when i < p -> p - 1
            | i::rest when i > p -> p + 1
            | _ -> p // empty or arrived
        // counter
        let rec loop n bs ob bb op bp =  // buttons, blue buttons, orange buttons, blue position, orange position
            match bs, ob, bb with
            | [], _, _ -> n // ready
            | (c,i)::bs, _::ob, _ when c = 'O' && op = i -> // orance in position and comes now
                loop (n+1) bs ob bb op (approach bb bp) // orange cannot change place, he has to stay and push the button, but his next target moved
            | (c,i)::bs, _, _::bb when c = 'B' && bp = i -> // blue in position
                loop (n+1) bs ob bb (approach ob op) bp
            | _ -> // none of them in position yet, both can move
                loop (n+1) bs ob bb (approach ob op) (approach bb bp)
        loop 0 buttons (colorButtons 'O') (colorButtons 'B') 1 1

    // transform list to pair list
    let pairs f lst =
        let rec loop lst = 
            match lst with
            | [] -> []
            | c::i::rest -> 
                (f c i)::(loop rest)
            | _::rest -> failwith "uneven number of elements"
        loop lst

    let solve fn = 
        solveFile fn (fun line ->
            line.Split( [|' '|] ) |> Array.toList 
            |> List.tail // skip first 
            |> pairs (fun c i -> char c, System.Int32.Parse(i)) // group to 
            |> walk |> string )     
            
    assertEqual (walk ['O',2; 'B',1; 'B',2; 'O',4]) 6 "walk"         
    assertEqual (walk ['O',5; 'O',8; 'B',100]) 100 "walk"
    assertEqual (walk ['B',2; 'B',1]) 4 "walk"

    //solve "bot-trust-sample.in"
    //solve "bot-trust-small-practice.in"
    //solve "bot-trust-large-practice.in"
                
                
// http://code.google.com/codejam/contest/975485/dashboard#s=p1
module Magicka = 
    // open Utils
    // convert a case to spell sets
    let lineToSpell (line: string) = 
        let next = line.Split( [|' '|] ) |> Array.toSeq |> makeNext
        let c = next() |> int      
        let comb = next |> take c
                    |> Seq.collect (fun abc -> [(abc.[0],abc.[1]), abc.[2]; 
                                                (abc.[1],abc.[0]), abc.[2]])
                    |> Map.ofSeq
        let d = next() |> int
        let opp = next |> take d
                    |> Seq.collect (fun ab -> [ab.[0],ab.[1]; 
                                               ab.[1],ab.[0]])
                    |> Set.ofSeq
        let n = next() |> int                  
        let spell = next()
        comb, opp, spell |> List.ofSeq
    // calculate the element list
    let invoke comb opp spell = 
        let rec loop spell elist eset = 
            match spell, elist with
            | [], elist -> elist |> List.rev  
            | h::t, e::eprev when comb |> Map.containsKey (h,e) -> // combination
                let elist = comb.[(h,e)]::eprev
                loop t elist (elist |> Set.ofList)
            | h::t, _ when eset |> Set.exists (fun e -> // opposition
                                    opp |> Set.exists (fun (a,b) ->
                                        a = h && b = e)) ->
                loop t [] Set.empty
            | h::t, elist ->
                loop t (h::elist) (eset |> Set.add h)
        loop spell [] Set.empty

    let invokeLine line = 
        let comb, opp, spell = line |> lineToSpell
        invoke comb opp spell

    assertEqual (invokeLine "0 0 2 EA") ['E';'A'] "invoke" 
    assertEqual (invokeLine "1 QRI 0 4 RRQR") ['R';'I';'R'] "invoke"
    assertEqual (invokeLine "1 QFT 1 QF 7 FAQFDFQ") ['F';'D';'T'] "invoke"
    assertEqual (invokeLine "1 EEZ 1 QE 7 QEEEERA") ['Z';'E';'R';'A'] "invoke"
    assertEqual (invokeLine "0 1 QW 2 QW") [] "invoke"

    let elementsToString (elist: char list) = 
        sprintf "[%s]" (System.String.Join(", ", elist))

    let solve fn = 
        solveFile fn (fun line ->
            line
            |> invokeLine
            |> elementsToString )

    //solve "magicka-sample.in"
    //solve "magicka-small-practice.in"
    //solve "magicka-large-practice.in"

// http://code.google.com/codejam/contest/975485/dashboard#s=p2&a=1
module CandySplitting = 
    // open Utils
    // first line is the number of candies in the second
    let linesToCandies (lines: string) =  
        lines.Split( [|'\n'|] ).[1].Split( [|' '|] ) |> Array.map int64
    // whether it is possible to divide into two groups is easy. the total XOR sum has to be zero
    let isSolvable candies = 
        candies |> Array.reduce (^^^) |> (=) 0L
    // if there is a solution, we can just give the least valuable and the rest will have to be the same worth, because if we XOR them together it will be zero
    let split candies = 
        if candies |> isSolvable |> not then
            None
        else // min ^^^ rest = 0 so let's just give him the minimum
            let s = candies |> Array.sum
            let m = candies |> Array.min 
            Some(s-m)

    let splitToString = function
        | None -> "NO"
        | Some(x) -> x |> string

    assertEqual (split [|1L;2L;3L;4L;5L|]) None "candy"
    assertEqual (split [|3L;5L;6L|]) (Some 11L) "candy"
    // solve a file
    let solve fn = 
        solveFile2 fn (linesToCandies >> split >> splitToString)

    //solve "candy-splitting-sample.in"
    //solve "candy-splitting-small-practice.in"
    //solve "candy-splitting-large-practice.in"



// http://code.google.com/codejam/contest/975485/dashboard#s=p3&a=4
module GoroSort = 
    // open Utils
    // selection sort can use 0..n-1 swaps but many comparisons to find the minimum
    let minIndex (arr : 'a array) ifrom ito =      
        let mutable mini = ifrom        
        for i in ifrom+1 .. ito do
            if arr.[i] < arr.[mini] then
                mini <- i
        mini

    // in place sort
    let selectionSort arr = 
        let n = arr |> Array.length
        let mutable swaps = 0
        for i in 0 .. n-2 do
            // assume sorted before i. find the minimum above
            let mini = minIndex arr i (n-1)
            if i < mini then
                let temp = arr.[i]
                arr.[i] <- arr.[mini]
                arr.[mini] <- temp
                swaps <- swaps + 1
        swaps

    // shuffle means 50% chance to swap elements so each needs in average 2 hits
    let averageHits swaps = 
        (float swaps) * 2.0

    // first line is the number of cards in the second
    let linesToArray (lines: string) =  
        lines.Split( [|'\n'|] ).[1].Split( [|' '|] ) |> Array.map int

    // solve a file
    let solve fn = 
        solveFile2 fn (linesToArray >> selectionSort >> averageHits >> sprintf "%.6f" )

    // wrong, this is not the solution. the expectation is that a random permutation puts 1 element in place

    //solve "gorosort-sample.in"
    //solve "gorosort-small-practice.in"
    //solve "gorosort-large-practice.in"    

