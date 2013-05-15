namespace CodeJam
open CodeJam.Utils
open System


// https://code.google.com/codejam/contest/2270488/dashboard
module TicTacToeTomek = 
    
    type Result = Draw | Won of char | NotCompleted       

    // enumerate all possible vectors
    let candidates table = seq {
        let maxi = (table |> Array2D.length1) - 1
        let toVec arr = 
            arr |> Array2D.toSeq |> Seq.map (fun (r,c,v) -> v) |> Seq.toArray
        // rows and columns
        for i in 0 .. maxi do
            yield table.[i .. i, 0 .. maxi] |> toVec
            yield table.[0 .. maxi, i .. i] |> toVec
        // diagonals
        yield [| for i in 0 .. maxi -> table.[i,i] |]
        yield [| for i in 0 .. maxi -> table.[i,maxi-i] |]
        }

    let outcome vec = 
        let cnt = vec |> Seq.countBy id |> Map.ofSeq
        let wins c = 
            match Map.tryFind c cnt with
            | Some(4) -> true
            | Some(3) -> Map.tryFind 'T' cnt = Some(1)
            | _ -> false
        if 'X' |> wins then Some(Won('X'))
        elif 'O' |> wins then Some(Won('O'))
        else None

    let check table = 
        let result = table 
                    |> candidates 
                    |> Seq.tryPick outcome
        match result with
        | Some(win) -> win
        | _ -> if table |> Array2D.toSeq |> Seq.exists (fun (r,c,v) -> v = '.') then NotCompleted else Draw

    let solve fn = 
        // there is a case every 5 lines
        solveFileBy (caseByN 5) fn <| fun s -> 
            let table = (splitLines s).[.. 3] 
                        |> Array.map (fun line -> [|for i in 0 .. 3 -> line.[i]|])
                        |> array2D
            table |> check |> (function 
                | Won(side) -> sprintf "%c won" side
                | Draw -> "Draw"
                | NotCompleted -> "Game has not completed")

    // CodeJam.TicTacToeTomek.solve "tictactoetomek-sample.in"


// https://code.google.com/codejam/contest/2270488/dashboard#s=p1
module Lawnmower = 

    type Direction = Up | Down | Left | Right

    let next dir (r,c) = 
        match dir with 
        | Up -> (r-1, c)
        | Down -> (r+1, c)
        | Left -> (r, c-1)
        | Right -> (r, c+1)
        
    let check table = 
        let maxr = (table |> Array2D.length1) - 1
        let maxc = (table |> Array2D.length2) - 1
        // we have to be able to go up&down or left&right from every cell not running into higher ground
        let rec maxh = memoize <| fun (dir, (r,c)) ->
            if  dir = Up && r = 0 
                || dir = Down && r = maxr 
                || dir = Left && c = 0 
                || dir = Right && c = maxc 
            then
                table.[r,c]
            else 
                let nr, nc = next dir (r,c)
                max table.[r,c] (maxh (dir,(nr,nc)))
        // check every cell that it's possible to get out
        table |> Array2D.toSeq |> Seq.forall (fun (r,c,v) -> 
            let h = table.[r,c]
            maxh (Up, (r,c)) <= h && maxh (Down, (r,c)) <= h || 
            maxh (Left, (r,c)) <= h && maxh (Right, (r,c)) <= h)
                
    let solve fn = 
        // there is a case every 5 lines
        solveFileHxW fn <| fun s -> 
            let table = (splitLines s)
                        |> Array.map (splitSpaces >> Array.map int)
                        |> array2D
            table |> check |> (function true -> "YES" | false -> "NO")

    // CodeJam.Lawnmower.solve "lawnmower-sample.in"



// https://code.google.com/codejam/contest/2270488/dashboard#s=p2
module FairAndSquare = 
    
    let isPalindrome n = 
        let chrs = string n
        let rec loop i = 
            if not(chrs.[i].Equals(chrs.[chrs.Length-1-i])) then
                false
            elif i = chrs.Length / 2 then
                true
            else
                loop (i+1)
        loop 0     

    let fairAndSquares (a: bigint) (b: bigint) = 
        // go up to the root of B to find palindromes, square them, check again
        let rec loop i = seq {            
            let ii = i * i
            if ii <= b then
                if ii >= a && isPalindrome i && isPalindrome ii then
                    yield ii                
                yield! loop (i+1I)
            }
        loop 1I

    let getFnS lst a b = 
        lst |> Seq.filter (fun i -> i >= a && i <= b) 

    let solve fn = 
        // there is a case every line
        // prepare a cached version
        let M = bigint.Pow(10I, 14) 
        let cache = fairAndSquares 1I M |> List.ofSeq
        let count = getFnS cache

        solveFile fn <| fun s -> 
            let [|a;b|] = s |>splitSpaces |> Array.map bigint.Parse
            count a b |> Seq.length |> string

    // CodeJam.FairAndSquare.solve "fairandsquare-sample.in"


// https://code.google.com/codejam/contest/2270488/dashboard#s=p3
module Treasure = 

    let hasKey key keys = 
        match keys |> Map.tryFind key with
        | Some(x) when x > 0 -> true
        | _ -> false

    let addKeys key n keys = 
        match keys |> Map.tryFind key with
        | Some(x) -> 
            keys |> Map.add key (x+n)
        | _ -> 
            keys |> Map.add key n

    let removeKey key keys = 
        match keys |> Map.tryFind key with
        | Some(x) when x > 0 -> 
            keys |> Map.add key (x-1)
        | _ -> 
            failwith "Cannot remove key %d from %A" key keys

    
    let traverse (keys: int list) (chests: (int * int * int list) list) =         
        // chest contents
        let contents = chests |> Seq.map (fun (id,typ,keys) -> (id, keys)) |> Map.ofSeq
        // key inventory
        let keyMap = keys |> Seq.countBy id |> Map.ofSeq
        // unopened chests
        let chestMap = chests |> Seq.map (fun (id,typ,keys) -> (id, typ)) |> Set.ofSeq

        // do a DFS on chests
        let rec loop ord ks cs = seq {
            if Set.isEmpty cs then
                yield ord |> List.rev
            // try to open any chest we have a key for
            for cid, typ in cs do
                if ks |> hasKey typ then
                    let cs' = cs |> Set.remove (cid,typ)
                    let ks' = ks |> removeKey typ
                    let ks' = contents.[cid] 
                                |> Seq.countBy id 
                                |> Seq.fold (fun keys (k,n) -> keys |> addKeys k n) ks'
                    yield! loop (cid::ord) ks' cs'
        }
        loop [] keyMap chestMap

    let solve fn = 
        // determine number of lines
        let getRows header = 
            let [|K;N|] = header |> splitSpaces |> Array.map int            
            1 + N 

        let solveLines ls = 
            let lines = splitLines ls

            // first line is the keys we start with
            let keys = lines.[0] |> splitSpaces |> Array.map int |> Array.toList

            // other lines are chests: type, number of keys, keys
            let chests = lines.[1 ..] 
                        |> Array.map (splitSpaces >> Array.map int)
                        |> Array.mapi (fun i arr ->
                            (i+1, arr.[0], arr.[2 ..] |> Array.toList))
                        |> Array.toList

            let solutions = traverse keys chests 
                            |> Seq.map (fun solution -> String.Join(" ", solution))
                            |> Seq.sort |> List.ofSeq

            if List.isEmpty solutions then "IMPOSSIBLE" else List.head solutions

        solveFileBy (caseByDyn getRows) fn solveLines
