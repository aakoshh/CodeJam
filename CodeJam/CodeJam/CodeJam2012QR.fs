namespace CodeJam

open CodeJam.Utils

module SpeakingTongues = 
    // add a pair to the character mapping
    let train mapping (encoded: string) (decoded: string) = 
        let join (s: char seq) = System.String.Join("", s)
        let rec loop map enc dec = 
            match enc, dec with
            | e::enc, d::dec ->
                match map |> Map.tryFind e with
                | Some(d') when d' <> d ->
                    printfn "%s -> %s" (join enc) (join dec)
                    failwithf "%c is already mapped to %c, not %c" e d' d 
                | _ -> 
                    let map = map |> Map.add e d
                    loop map enc dec
            | [], [] ->
                map
            | _ -> failwith "uneven length"
        loop mapping (List.ofSeq encoded) (List.ofSeq decoded)

    let encode (mapping: Map<char,char>) s = 
        s |> String.map (fun c -> mapping.[c])

    let known = [//"a zoo", "y qee";
                 "ejp mysljylc kd kxveddknmc re jsicpdrysi", "our language is impossible to understand";
                 "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd","there are twenty six factorial possibilities";
                 "de kr kd eoya kw aej tysr re ujdr lkgc jv","so it is okay if you want to just give up"]

    let map = known |> List.fold (fun map (enc,dec) -> train map enc dec) Map.empty

    assertEqual (known |> List.forall (fun (enc,dec) -> dec = encode map enc)) true "speaking"

    // analyze
    // map |> Map.toSeq |> Seq.sort |> Seq.iter (fun (e,d) -> printfn "%c -> %c" e d)
    // map |> Map.toSeq |> Seq.sortBy snd |> Seq.iter (fun (e,d) -> printfn "%c -> %c" e d)
    // q and z missing from both, so either to themselves or to each other

    let map1 = train map "qz" "qz"
    let map2 = train map "qz" "zq" // in attemp0 there is a case mapping the abc that tells

    let solve fn = 
        solveFile fn (encode map2)

    //solve "speaking-tongues-sample.in"
    //solve "speaking-tongues-small-attempt1.in"



module RecycledNumbers = 
    // generate pairs composable from n
    let pairs n = 
        if n < 10 then
            Set.empty
        else
            let ns = n |> string
            seq{
            for i in 1 .. ns.Length-1 do
                let m = (ns.Substring(i) + ns.Substring(0,i)) |> int
                if (string m).Length = ns.Length then
                    yield (n, m)} |> Set.ofSeq

    // count the pairs between A and B so that A ≤ n < m ≤ B
    let count a b = 
        let rec loop n ps = 
            if n > b then
                ps
            else
                let nps = pairs n |> Set.filter (fun (n,m) -> a <= m && n < m && m <= b)
                // the pairs will show up for each recycle
                let ps = Set.union ps nps
                loop (n+1) ps
        loop a Set.empty |> Set.count

    let collect a b = 
        let rec loop n ps = 
            if n > b then
                ps
            else
                let nps = pairs n |> Set.filter (fun (n,m) -> a <= m && n < m && m <= b)
                // the pairs will show up for each recycle
                let ps = Set.union ps nps
                loop (n+1) ps
        loop a Set.empty
    (* // larege needs caching
    let ps = collect 1 2000000 |> Set.toList
    let count2 a b = 
        ps |> Seq.filter (fun (n,m) -> 
            a <= m && n < m && m <= b && a <= n && n <= b)
           |> Seq.length
    *)

    assertEqual (count 1 9) 0 "recycle"
    assertEqual (count 10 40) 3 "recycle"
    assertEqual (count 100 500) 156 "recycle"
    assertEqual (count 1111 2222) 287 "recycle"

    let solve fn = 
        solveFile fn (fun line ->
            let [|a;b|] = line |> splitSpaces |> Array.map int
            count a b |> string)


    //solve "recycled-numbers-sample.in"
    //solve "recycled-numbers-small-attempt0.in"
    //solve "recycled-numbers-large.in"



module DancingWithGooglers = 
    // decompose without surprise
    let withoutSurprise total = 
        let avg = total / 3
        match total % 3 with
        | 0 -> [|avg;avg;avg|]
        | 1 -> [|avg;avg;avg+1|]
        | 2 -> [|avg;avg+1;avg+1|]

    let withSurprise total = 
        if total <= 1 then 
            [|0;0;total|] // avoid avg-1
        else
            let avg = total / 3
            match total % 3 with
            | 0 -> [|avg-1;avg;avg+1|]
            | 1 -> [|avg-1;avg+1;avg+1|]
            | 2 -> [|avg;avg;avg+2|]

    // number of dancers possibly with best point p
    let maxBetter p (s:int) totals = 
        // how many could have done it without a surprise?
        let scores = totals |> Array.map (fun t ->
                        (t, withoutSurprise t, withSurprise t))
        let easy,maybe = scores |> Array.partition (fun (_, ss, _) ->
                        ss |> Array.max >= p)
        // how many more could have done it with a surprise
        let withS = maybe |> Array.filter (fun (_, _, ss) ->
                        ss |> Array.max >= p)
        // count the cases
        (Array.length easy) + (min (Array.length withS) s )


    assertEqual (maxBetter 5 1 [|15; 23; 11|]) 3 "dancing"
    assertEqual (maxBetter 8 0 [|23; 22; 21|]) 2 "dancing"
    assertEqual (maxBetter 1 1 [|8; 0|]) 1 "dancing"
    assertEqual (maxBetter 8 2 [|29; 20; 8; 18; 18; 21|]) 3 "dancing"


    let solve fn = 
        solveFile fn (fun line ->
            let vals = line |> splitSpaces |> Array.map int
            let n = vals.[0]
            let s = vals.[1]
            let p = vals.[2]
            let t = vals.[3..]
            maxBetter p s t |> string)

    //solve "dancing-sample.in"
    //solve "dancing-small-attempt0.in"
    //solve "dancing-large.in"


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



module DiamondInheritence = 
    exception DiamondException of int * Set<int>
    // take list of base classes and detect diamonds
    let detectDiamonds bases = 
        // for each class i the map of bases
        let n = bases |> List.length
        let bases = bases |> List.mapi (fun i bs -> (i+1,bs |> Set.ofList)) |> Map.ofList
        // get the set of all base classes for a child
        let rec baseClasses = memoize (fun x -> 
            // add x to the set of bases of its parents
            // detect overlaps
            let rec loop parents found = 
                match parents with
                | [] -> found // set of all bases
                | p::parents ->
                    // base classes of parent
                    let bsp = baseClasses p
                    //printfn "%d = %A + %d->%A" x found p bsp
                    // raise exception if there is an intersection with found
                    if not (Set.intersect bsp found |> Set.isEmpty) then
                        raise( DiamondException(x, Set.intersect bsp found) )
                    // gather the rest
                    loop parents (Set.union bsp found)
            let parents = bases.[x] |> Set.toList
            // the first known base is self
            loop parents (Set.singleton x)
        )
        let rec detect i = 
            match i with 
            | i when i > n -> // past the last
                None // no diamond
            | i ->
                try 
                    baseClasses i |> ignore
                    detect (i+1)
                with 
                | DiamondException(x,bs) ->
                    Some(x,bs)
        detect 1

    let toAnswer = function
        | Some(_) -> "Yes"
        | None -> "No"


    assertEqual (detectDiamonds [[2];[3];[]]) None "diamond"
    assertEqual (detectDiamonds [[2;3];[4];[5];[5];[]]) (Some(1,set [5])) "diamond"
    assertEqual (detectDiamonds [[2;3];[3];[]]) (Some(1,set [3])) "diamond"


    let solve fn = 
        solveFileN fn (fun lines ->
            let bases = lines |> splitLines |> Seq.ofArray 
                              |> Seq.map (splitSpaces >> Array.map int >> List.ofArray >> List.tail) 
                              |> List.ofSeq
            bases |> detectDiamonds |> toAnswer)


    // solve "diamond-inheritence-sample.in"
    // solve "diamond-inheritence-small-attempt0.in"
    // solve "diamond-inheritence-large.in"



module BoxFactory = 

    // find the maximum number of matchings
    let optimize boxes toys = 
        let rec loop = memoize (fun ((a: (int64*int64) list), (b: (int64*int64) list)) ->
            match a, b with
            | (0L,_)::resta, _ -> // ran out of current a
                loop (resta,b)
            | _, (0L,_)::restb -> // ran out of current b
                loop (a,restb)
            | [], _ | _, [] -> // ran out of production
                0L // nothing left to produce
            | (na,ta)::resta, (nb,tb)::restb when ta = tb -> // same type, we can produce the minimum amount
                let p = min na nb
                let s = loop ((na-p,ta)::resta, (nb-p,tb)::restb)
                p + s
            | (na,ta)::resta, (nb,tb)::restb -> // different types so we must discard one
                let sa = loop (resta,b)
                let sb = loop (a,restb)
                max sa sb )
        loop (boxes,toys)


    assertEqual (optimize [10L,1L; 20L,2L; 25L,3L] [10L,2L; 30L,3L; 20L,1L]) 35L "boxfactory"
    assertEqual (optimize [10L,1L; 6L,2L; 10L,1L] [5L,1L; 3L,2L; 10L,1L; 3L,2L; 5L,1L]) 20L "boxfactory"
    assertEqual (optimize [10L,1L; 6L,2L; 10L,1L] [5L,1L; 6L,2L; 10L,1L; 6L,2L; 5L,1L]) 21L "boxfactory"
    assertEqual (optimize [5000000L,10L] [5000000L,100L]) 0L "boxfactory"
    
    let solve fn = 
        (solveFileBy (caseByN 3)) fn (fun lines ->
            let input = lines |> splitLines 
                              |> Array.map (splitSpaces >> Array.map int64) 
            let n,m = int(input.[0].[0]), int(input.[0].[1])
            let a = [0 .. n-1] |> List.map (fun i -> 
                        input.[1].[2*i], input.[1].[2*i+1])
            let b = [0 .. m-1] |> List.map (fun i -> 
                        input.[2].[2*i], input.[2].[2*i+1])
            optimize a b |> string )


    // solve "box-factory-sample.in"
    // solve "box-factory-small-attempt0.in"
    // solve "box-factory-large.in"



module OutOfGas = 

    let descend d (pos: float[][]) acc = 
        // when does the other car reach distance d
        let rec loop i = 
            if pos.[i].[1] >= d then
                let s = if i = 0 then 0.0 else pos.[i-1].[1]
                let t = if i = 0 then 0.0 else pos.[i-1].[0]
                (d-s) / (pos.[i].[1]-s) * (pos.[i].[0]-t)
            else 
                loop (i+1)
        let tcar = loop 0
        // see if we can be as fast as him
        // if not, just follow
        tcar


    let solve fn = 
        let solver = solveFileBy (caseByDynWithHeader (fun pline -> // the header line tells that an H x W matrix follows
                                    1 + (int (pline.Split([|' '|]).[1]))))
        solver fn (fun lines ->
            let input = lines |> splitLines
                              |> Array.map (splitSpaces >> Array.map float)
            let D = input.[0].[0] // distance from house
            let N = input.[0].[1] |> int
            let pos = input.[1 .. N] // (t,x)
            let A = input.[N+1]
            descend D pos A |> (sprintf "%.6f"))


    // solve "out-of-gas-sample.in"