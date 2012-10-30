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


// http://code.google.com/codejam/contest/90101/dashboard#s=p2
module WelcomeToCodeJam =
    // open Utils
    // remove superfluous characters
    let clean (search: string) (line: string) = 
        let ss = search |> Set.ofSeq
        let fl = line |> Seq.filter (fun s -> ss |> Set.contains s)
        System.String.Join("", fl)
    // count how many times a string s appears in a line
    let count search line =         
        let search = search |> List.ofSeq
        let line = line |> List.ofSeq
        // count lists
        let rec loop ss cs cont = 
            match ss, cs with
            | [], _ -> cont 1 // got a match
            | _, [] -> cont 0 // exhausted the long line and there is nothing
            | s::st, c::ct when s = c -> // so the first characters match, we can pop off one and descend
                loop st ct <| (fun cnt1 -> // we know what the result of popping off, now see what happens if we try top match later
                    loop ss ct <| (fun cnt2 ->
                        cont (cnt1+cnt2))) // propagate the result of both ways
            | s::st, c::ct ->  // the first characters did not match
                loop ss ct cont // drop that letter and try the next
        loop search line id    

    // count is too slow for large data, try memoization
    let countm (search: string) (line: string) =         
        let msi = search.Length - 1
        let msc = line.Length - 1
        // count by index
        let rec loop = memoize( fun (si, ci) -> // comparing index 
            //printfn "(%d,%d) %s vs %s" si ci (search.Substring(si)) (line.Substring(ci))
            if si > msi then 
                1 // reached the end, one match
            elif ci > msc then 
                0 // past the line without match
            elif search.[si] = line.[ci] then // a matching character, now we can try to pop/not pop, popping later will lead to memoized results
                let cnt1 = loop (si+1, ci+1) // pop
                let cnt2 = loop (si, ci+1) // pop later
                cnt1 + cnt2
            else // not matching
                loop (si, ci+1) )
        loop (0,0)

    // only report the last 4 digits
    let report i = 
        let s = (sprintf "%04d" i)
        s.Substring( s.Length-4, 4 )

    let search = "welcome to code jam"        

    assertEqual (countm search "elcomew elcome to code jam") 1 "welcome"
    assertEqual (countm search "wweellccoommee to code qps jam") 256 "welcome"
    assertEqual (countm search "welcome to codejam") 0 "welcome"
    assertEqual (countm search "So you've registered. We sent you a welcoming email, to welcome you to code jam. But it's possible that you still don't feel welcomed to code jam. That's why we decided to name a problem welcome to code jam. After solving this problem, we hope that you'll feel very welcome. Very welcome, that is, to code jam.") 400263727 "welcome"

    let solve fn = 
        solveFile fn (fun line ->
            countm search line |> report)

    //solve "welcome-sample.in"
    //solve "welcome-small-practice.in"
    //solve "welcome-large-practice.in"


// http://code.google.com/codejam/contest/90101/dashboard#s=p1
module WaterSheds = 
    // open Utils          
    // assign basin labels
    let drain (arr: int [,]) = 
        let nextbasin = makeNext ['a' .. 'z']
        let maxr = arr |> Array2D.length1
        let maxc = arr |> Array2D.length2
        // get the lowest neigbour of a cell
        let lowest (r,c) =             
            let (_,_,r,c) = 
                [(0, r, c); (1, r-1, c); (2, r, c-1); (3, r, c+1); (4, r+1, c)] // _ N W E S with the default ordering
                |> Seq.filter (fun (_, r, c) -> r >= 0 && r < maxr && c >= 0 && c < maxc) // not off the map
                |> Seq.map (fun (n, r, c) -> (arr.[r,c], n, r, c)) // add the height at the place
                |> Seq.sort // sort by height then default
                |> Seq.head
            r,c // return coordinates
        // the label of a tile is the same as its lowest neigbour, so once assigned it can be propagated        
        let rec label = memoize (fun (r,c) ->
            // where does it flow?
            match lowest (r,c) with
            | mr,mc when mr = r && mc = c -> nextbasin()
            | tile -> label tile)
        // if we go from top-left to bottom right the labels will be in lexicographical order
        //arr |> Array2D.mapi (fun r c _ -> lowest (r,c))
        arr |> Array2D.mapi (fun r c _ -> label (r,c))     
        
    let mapToString (arr: char [,]) = 
        let builder = new System.Text.StringBuilder()
        let maxc = arr |> Array2D.length2
        let append r c (d: char) = 
            if c = 0 then
                builder.Append("\n") |> ignore
            builder.Append(d) |> ignore
            if c < maxc-1 then
                builder.Append(" ") |> ignore
        arr |> Array2D.iteri append
        builder.ToString()                   
    
    assertEqual (array2D [[9;6;3];[5;9;6];[3;5;9]] |> drain) (array2D [['a';'b';'b'];['a';'a';'b'];['a';'a';'a']]) "drain"

    let solve fn = 
        solveFileHxW fn (fun lines -> // string map
            lines |> splitLines |> Array.map splitSpaces |> array2D |> Array2D.map int |> drain |> mapToString)

    //solve "watersheds-sample.in"
    //solve "watersheds-small-practice.in"
    //solve "watersheds-large-practice.in"



// http://code.google.com/codejam/contest/90101/dashboard#s=p0
module AlienLanguage = 
    // open Utils
    open System.IO

    // break down a pattern into L parts
    let groups (pattern: string) = 
        let rec loop isg xs accu = 
            match xs, accu with 
            | [], _ -> // no more patterns
                accu |> List.rev
            | ')'::xs, _ -> // end of a group
                loop false xs accu
            | '(':: xs, _ -> // start of a group
                loop true xs ([]::accu)
            | x::xs, curr::accu when isg -> // append to current group
                loop isg xs ((x::curr)::accu)
            | x::xs, _ -> // new group
                loop isg xs ([x]::accu)
        loop false (pattern |> List.ofSeq) [] |> List.map List.rev

    assertEqual ("(zyx)bc" |> groups) [['z'; 'y'; 'x']; ['b']; ['c']] "alien"

    // the list of possible words (could have used regular expressions by replacing () with []) 
    let possibilities words line = 
        // eliminate words. if using lists would be too slow, we could prepare a set of words per letter position
        // like words having 'a' in first pos
        let rec loop i (words: string list) groups = 
            match groups with
            | [] -> words
            | g::groups -> 
                // filter words to those only whose i-th character is in the group
                let words = words |> List.filter (fun word -> Set.contains word.[i] g)
                loop (i+1) words groups
        let groups = line |> groups |> List.map Set.ofList
        loop 0 words groups

    let possibilityCount words line = possibilities words line |> List.length

    open System.Text.RegularExpressions

    let countRegex words (line: string) = 
        let pattern = line.Replace('(','[').Replace(')',']')
        words 
        |> Seq.filter (fun w -> Regex.IsMatch(w, pattern))
        |> Seq.length

    
    let words = ["abc";"bca";"dac";"dbc";"cba"]

    assertEqual (countRegex words "(ab)(bc)(ca)") 2 "alien"
    assertEqual (countRegex words "abc") 1 "alien"
    assertEqual (countRegex words "(abc)(abc)(abc)") 3 "alien"
    assertEqual (countRegex words "(zyx)bc") 0 "alien"

    // totally unique file structure
    let solve fileName = 
        let fni = fileName |> toPath
        let fno = Path.GetFileNameWithoutExtension(fileName)+".out" |> toPath
        use writer = new StreamWriter( fno )
        // read lines and solve them
        let lines = File.ReadAllLines(fni) |> List.ofArray
        // first line contains word length, number of words, number of tests
        let [|l;d;n|] = lines |> List.head |> splitSpaces |> Array.map int
        // get the words
        let words = lines |> Seq.skip 1 |> Seq.take d |> List.ofSeq
        // solve the cases
        lines 
        |> Seq.skip (1+d)
        |> Seq.map ((possibilityCount words) >> string)
        |> Seq.iteri (fun i solution ->
            writer.WriteLine( sprintf "Case #%d: %s" (i+1) solution )
            writer.Flush()) // write early so we can see where it halted if its slow
        writer.Close()
        fno // to see that it is ready


    //solve "alien-language-sample.in"
    //solve "alien-language-small-practice.in"
    //solve "alien-language-large-practice.in"


// http://code.google.com/codejam/contest/837485/dashboard
module ClosingTheLoop = 
    // open Utils

    // given a list of segments, find the longest loop
    let close segments = 
        // the segments will need to be paired up
        let blue,red = segments |> List.sortBy (fun (l,c) -> -l)
                                |> List.partition (fun (l,c) -> c = 'B')
        // the longest loop consists of segments up to the shorter list of the two
        let minl = min (blue |> List.length) (red |> List.length)
        // length of the first n
        let len s = 
            s |> Seq.take minl |> Seq.map fst |> Seq.sum
        (len blue) + (len red) - minl * 2 // each segment will lose 1 cm to knots, there are 2*minl segments

    
    assertEqual (close [5,'B']) 0 "close-loop"
    assertEqual (close [6,'R'; 1,'B'; 7,'R'; 3,'B']) 13 "close-loop"
    assertEqual (close [5,'B'; 4,'R'; 3,'R'; 2,'R'; 5,'R'; 4,'R'; 3,'R']) 8 "close-loop"
    assertEqual (close [20,'B'; 20,'R']) 38 "close-loop"

    // solve a case
    let linesToSegments (lines: string) =
        (lines |> splitLines).[1]  // first line is the number of segments
            |> splitSpaces 
            |> Array.map (fun lc -> 
                (lc.Substring(0,lc.Length-1) |> int, lc.[lc.Length-1])) // each segment becomes a tuple of (length,color)        
            |> List.ofArray

    let solve fn = 
        solveFile2 fn (linesToSegments >> close >> string)

    //solve "closing-the-loop-sample.in"
    //solve "closing-the-loop-small-practice.in"
    //solve "closing-the-loop-large-practice.in"



// http://code.google.com/codejam/contest/837485/dashboard#s=p1
module InvestingTheMarket = 
    // open Utils

    // given a list of segments, find the longest loop
    let invest (money, prices) = 
        // choose the highest investment
        let choices = seq {
            let n = prices |> Array.length
            for i in 0 .. n-2 do
                for j in i+1 .. n-1 do
                    // yield profit, initial investment, month indices
                    let bought = money / prices.[i] // maybe zero
                    yield ( (prices.[j] - prices.[i]) * bought, // profit
                            prices.[i],  // price per unit
                            i+1, j+1 )
        }
        // choose the best
        let best = choices |> Seq.maxBy (fun (p,i,_,_) -> (p,-i))
        match best with
        | p,_,_,_ when p <= 0 -> "IMPOSSIBLE"
        | p,i,m1,m2 -> 
            sprintf "%d %d %d" m1 m2 p

    
    assertEqual (invest (100,[|1;2;3;4;5;6;7;8;9;10;11;12|])) "1 12 1100" "invest"
    assertEqual (invest (100,[|52;50;25;100;61;63;70;51;71;55;10;5|])) "3 4 300" "invest"
    assertEqual (invest (100,[|200;150;250;132;125;110;210;220;180;176;108;113|])) "IMPOSSIBLE" "invest"

    // solve a case
    let linesToInvestment (lines: string) =
        let lines  = (lines |> splitLines) // first the money, then the values
        let money  = lines.[0] |> int
        let prices = lines.[1] |> splitSpaces |> Array.map int
        (money,prices)

    let solve fn = 
        solveFile2 fn (linesToInvestment >> invest )

    //solve "investing-sample.in"
    //solve "investing-small-practice.in"
    //solve "investing-large-practice.in"


// http://code.google.com/codejam/contest/837485/dashboard#s=p2&a=1
module BuildingHouse = 
    // open Utils

    let build map =         
        // G and S can be used, the others are obstacles
        let isObstacle = function 
            | 'G' | 'S' -> false | _ -> true
        // find the largest rectangular area
        let rec loop (r,c) map =         
            let h,w = map |> Array2D.length1, map |> Array2D.length2
            // need to check for obstacles
            if isObstacle map.[r,c] then // we can try to look in four planes around the obstacle
                let planes = [0,0,h-1,c-1; 0,0,r-1,w-1; r+1,0,h-1,w-1; 0,c+1,h-1,w-1] 
                                |> List.filter (fun (rt,ct,rb,cb) -> 
                                       0 <= rt && rt < h && 0 <= ct && ct < w 
                                    && 0 <= rb && rb < h && 0 <= cb && cb < w )
                                |> List.map (fun (rt,ct,rb,cb) ->
                                    let arr = map.[rt..rb,ct..cb]
                                    loop (0,0) arr)
                match planes with 
                | [] -> 0 // probably was already a 1x1 cell
                | _ -> planes |> List.max
            elif r = h-1 && c = w-1 then
                h * w // completed square
            else // not an obstacle, look further 
                let (r,c) = if c+1 < w then (r,c+1) else (r+1,0) // next cell to check
                loop (r,c) map
        loop (0,0) map
        
    assertEqual (build (array2D [['G']])) 1 "building"
    assertEqual (build (array2D [['G';'S'];['S';'G']])) 4 "building"
    assertEqual (build (array2D [['G';'T'];['G';'G']])) 2 "building"
    assertEqual ("GGTGG
TGGGG
GSSGT
GGGGT
GWGGG
RGTRT
RTGWT
WTWGR" |> splitLines |> Array.map Array.ofSeq |> array2D |> build) 9 "building"

    let solve fn = 
        solveFileWxH fn (fun lines -> // string map
            lines 
            |> splitLines 
            |> Array.map Array.ofSeq
            |> array2D |> build |> string)

    //solve "building-house-sample.in"
    //solve "building-house-small-practice.in"
    //solve "building-house-large-practice.in"

    [1;2;2] |> List.filter((=) 0) |> List.max


