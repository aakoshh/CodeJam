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
