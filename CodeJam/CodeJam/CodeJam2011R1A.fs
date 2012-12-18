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




module KillerWord = 
    
    /// try a character and substitute into the array (by mutation)
    let guess (solution: char[]) (mystery: Option<char>[]) c = 
        let n = Array.length solution
        let mutable success = false
        for i in 0 .. n-1 do
            if solution.[i] = c then
                success <- true
                mystery.[i] <- Some(c)
        success, mystery

    /// eliminate choices from the dictionary if they don't fit the current facts
    let eliminate (D: Set<char[]>) (mystery: Option<char>[]) (wrong: Set<char>) = 
        let n = Array.length mystery
        // filter the dictionary
        D |> Set.filter (fun d ->
            // length must be the same
            let nd = Array.length d
            n = nd && Array.forall2 (fun dc mc -> 
                // and every guessed character has to match, or if not fixed then must not be in previous guesses
                match mc with 
                | None -> wrong |> Set.contains dc |> not
                | Some(c) -> dc = c
                ) d mystery )
    

    /// work out the points of a solution to a word using a dictionary and a list of characters
    let hangman (word: string) words guesses = 
        let solution = word |> Array.ofSeq
        let mystery = solution |> Array.map (fun _ -> None)
        let solved () = 
            mystery |> Array.exists (fun x -> x.IsNone) |> not
        let rec loop words guesses wrong score = 
            match guesses with 
            | c::rest ->
                // see if the character is in any of the words
                let viable = words |> Set.exists (Array.exists ((=) c))
                if viable then
                    // guessing mutates the mystery
                    match guess solution mystery c with
                    | true, mystery -> 
                        if solved() then
                            score
                        else
                            let words' = eliminate words mystery wrong
                            loop words' rest wrong score
                    | false, mystery ->
                        // didn't substitute any characters
                        let wrong' = wrong |> Set.add c
                        let words' = eliminate words mystery wrong'
                        loop words' rest wrong' (score-1)
                else // this character cannot be used, but that doesn't lower our score
                    let wrong' = wrong |> Set.add c
                    loop words rest wrong' score
            | [] -> // should not happen 
                failwith (sprintf "Ran out of guesses at %A" mystery)
        loop words guesses Set.empty 0


    /// get the first word that gives the worst score
    let hardest words guesses = 
        let score, _, word = 
            words 
            |> Set.toSeq 
            |> Seq.mapi (fun i w -> 
                let word = new System.String(w)
                let score = hangman word words guesses
                score, i, word)
            |> Seq.minBy (fun (s,i,w) -> (s,i))
        word
    

    let test() = 
        let D = ["banana";"caravan";"pajamas"] |> List.map Array.ofSeq |> Set.ofList
        let L = "abcdefghijklmnopqrstuvwxyz" |> List.ofSeq
        let score = hangman "pajamas" D L
        assertEqual score -1 "killerword"
        let hard = hardest D L
        assertEqual hard "pajamas"


    /// transform a block of strings to the dictionary and guesses
    let toCase lines = 
        let ls = lines |> splitLines
        let [|N; M|] = ls.[0] |> (splitSpaces >> Array.map int)
        let D = ls.[1..N] |> Array.map Array.ofSeq |> Set.ofArray
        let L = ls.[(N+1)..] |> Array.map List.ofSeq
        D, L


    let solve fn = 
        // create solver that reads N + M lines
        let solver = 
            solveFileBy (caseByDynWithHeader (fun pline -> // the header line tells that an H x W matrix follows
                let [|N;M|] = pline.Split([|' '|]) |> Array.map int
                N + M ))

        solver fn (fun lines -> 
            let D,Ls = lines |> toCase
            // output all words on the same line
            let solutions = Ls |> Array.map (fun L -> hardest D L) |> joinSpaces
            solutions)


            