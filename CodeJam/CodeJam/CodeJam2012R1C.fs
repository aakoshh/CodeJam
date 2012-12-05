namespace CodeJam
open CodeJam.Utils


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