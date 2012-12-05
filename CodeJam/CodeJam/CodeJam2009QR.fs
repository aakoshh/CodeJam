namespace CodeJam
open CodeJam.Utils



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

