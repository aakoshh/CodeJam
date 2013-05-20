namespace CodeJam

module Utils = 
    let assertEqual x y msg = 
        if x <> y then
            failwith (sprintf "%s failed test. %A should be %A" msg x y)

    open System.Collections.Generic

    let memoize f = 
        let cache = Dictionary<_,_>(HashIdentity.Structural)
        fun args ->
            match cache.TryGetValue(args) with
            | (true, v) -> v
            | _ ->  let v = f(args)
                    cache.Add(args, v)
                    v

    // enumerator
    let makeNext (s: seq<'a>) =             
        let items = s.GetEnumerator()
        (fun () -> ignore (items.MoveNext()); items.Current)
    
    // take a couple of elements form a list
    let take n next = 
        let rec loop n accu = 
            if n = 0 then 
                accu |> List.rev
            else
                loop (n-1) (next()::accu)
        loop n []
    
    // partition a sequence into n-element chunks
    let rec partition n s = seq {
        if s |> Seq.isEmpty |> not then
            yield s |> Seq.take n // a normal sequence is not mutated by this
            yield! s |> Seq.skip n |> partition n }
    
    // sometimes the lines are paritioned in a truly dynamic way. first the number of elements, then so many follow, then number again
    let rec dynamicPartitions getn s = seq {
        if s |> Seq.isEmpty |> not then
            let n = (Seq.head s).ToString() |> getn // can be more than one number
            yield s |> Seq.skip 1 |> Seq.take n
            yield! s |> Seq.skip (n+1) |> dynamicPartitions getn }

    // sometimes the header row contains more
    let rec dynamicPartitionsWithHeader getn s = seq {
        if s |> Seq.isEmpty |> not then
            let n = (Seq.head s).ToString() |> getn // can be more than one number
            yield s |> Seq.take (1+n)
            yield! s |> Seq.skip (1+n) |> dynamicPartitionsWithHeader getn }


    /// dealing with the input / output of problems
    [<AutoOpen>]    
    module Solvers = 
        open System
        open System.IO
        // file path    
        let toPath fn = 
            if File.Exists(fn) then
                Path.GetFullPath(fn)
            else
                Path.Combine(__SOURCE_DIRECTORY__, "data", fn)

        let splitLines (s: string) = s.Split([|'\n'|])
        let splitSpaces (s: string) = s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        let joinSpaces (s: string[]) = String.Join(" ", s)
        let joinLines (s: string[]) = String.Join("\n", s)

        // in most problems the cases come line by line  
        let caseByOne (lines: seq<string>) = lines
        // in some however one case spans several lines. this function can be used if the number of lines per case is fixed   
        let caseByN n (lines: seq<string>) = 
            lines 
            |> Seq.cache // textreader consumes the interator and even the Seq.isEmpty will read to end it seems
            |> partition n 
            |> Seq.map (fun parts -> System.String.Join("\n", parts))
        // dynamic partitioning. this can be used if the partitioning can be determined from the rows
        let caseByDyn getn (lines: seq<string>) = 
            lines 
            |> Seq.cache 
            |> dynamicPartitions getn
            |> Seq.map (fun parts -> System.String.Join("\n", parts))

        let caseByDynWithHeader getn (lines: seq<string>) = 
            lines 
            |> Seq.cache 
            |> dynamicPartitionsWithHeader getn
            |> Seq.map (fun parts -> System.String.Join("\n", parts))

        // generic file solver
        let solveFileBy caseByLine fileName solver = 
            let fni = fileName |> toPath
            let fno = Path.Combine(Path.GetDirectoryName(fni), Path.GetFileNameWithoutExtension(fni)+".out")
            use writer = new StreamWriter( fno )
            // read lines and solve them
            File.ReadLines(fni)
            |> Seq.skip 1 // no of cases 
            |> caseByLine // gather multiple lines to one string if necessary
            |> Seq.map solver
            |> Seq.iteri (fun i solution ->
                writer.WriteLine( sprintf "Case #%d: %s" (i+1) solution )
                writer.Flush()) // write early so we can see where it halted if its slow
            writer.Close()
            fno // to see that it is ready
        // the usual case 
        let solveFile = solveFileBy caseByOne
        let solveFile2 = solveFileBy (caseByN 2)
        let solveFileN = solveFileBy (caseByDyn int) // each header row tells how many rows follow in the case


        // special line format for matrices
        let solveFileHxW = 
            solveFileBy (caseByDyn (fun pline -> // the header line tells that an H x W matrix follows
                pline.Split([|' '|]).[0] |> int)) // we can ignore W, just the number of rows is interesting

        let solveFileWxH = 
            solveFileBy (caseByDyn (fun pline -> // the header line tells that an W x H matrix follows
                pline.Split([|' '|]).[1] |> int)) // H is the second now


    [<AutoOpen>]
    module Extensions = 

        // extend Array2D
        module Array2D = 
            // add toSeq
            let toSeq arr = seq {
                for r in 0 .. (arr |> Array2D.length1) - 1 do
                    for c in 0 .. (arr |> Array2D.length2) - 1 do
                        yield (r,c, arr.[r,c]) }


        module Array =
            /// cumulate a function over the values of an array
            let cumulate f arr = 
                let cpy = arr |> Array.copy
                for i in 1 .. cpy.Length-1 do
                    cpy.[i] <- f cpy.[i] arr.[i-1]
                cpy

            /// sweep an array from left then right then combine the results excluding every i element
            let excluding cum comb arr = 
                let l2r = arr |> cumulate cum
                let r2l = arr |> Array.rev |> cumulate cum |> Array.rev
                let n = arr.Length
                arr |> Array.mapi (fun i x ->
                    let l = if i = 0   then None else Some(l2r.[i-1])
                    let r = if i = n-1 then None else Some(r2l.[i+1])
                    comb l x r )

            /// calcualte the minimum of other elements in the array 
            let minimums arr = 
                arr |> excluding min (fun l x r -> 
                        match l,r with
                        | Some(l), Some(r) -> min l r
                        | Some(l), _ -> l
                        | _, Some(r) -> r
                        | _ -> 0.0 ) 


    module Numerics = 
        open System

        // try to find the square root of an integer assuming that it will be an integer too
        let sqrti (i: bigint) = 
            let rec loop r = 
                if r * r = i then
                    r
                else
                    let r' = (r + i/r) / 2I
                    if r = r' then r else loop r'
            loop 1I