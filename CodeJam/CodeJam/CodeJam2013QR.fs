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