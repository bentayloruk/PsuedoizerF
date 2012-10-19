//THIS IS A VERY LITERAL TRANSLATION OF https://github.com/shanselman/Psuedoizer
//IT IS NOT INTENDED AS A GOOD EXAMPLE OF FUNCTIONAL PROGRAMMING IN F#

///Takes an English resource file (resx) and creates an artificial
///but still readable Euro-like language to exercise your i18n code
///without a formal translation.

type Person = { Name:string; Surname:string; }

//Usage message.
let usageMsg = "
Purpose: Takes an English resource file (resx) and creates an artificial
         but still readable Euro-like language to exercise your i18n code
         without a formal translation.

Psuedoizer.exe infile outfile
    Example:
    Psuedoizer.exe strings.en.resx strings.ja-JP.resx
"
//Exit codes. (Enum)
type ExitCode = | OK = 0 | BadArgCount = 1 | NoTextResourcesFound = 2 | InFileNameNotResx = 3

//Char conversions (tuple list to map).
let charMap =
    [   ('A', 'Å'); ('B', 'ß'); ('C', 'C'); ('D', 'Đ'); ('E', 'Ē'); ('F', 'F'); ('G', 'Ğ'); ('H', 'Ħ'); ('I', 'Ĩ'); ('J', 'Ĵ');
        ('K', 'Ķ'); ('L', 'Ŀ'); ('M', 'M'); ('N', 'Ń'); ('O', 'Ø'); ('P', 'P'); ('Q', 'Q'); ('R', 'Ŗ'); ('S', 'Ŝ'); ('T', 'Ŧ');
        ('U', 'Ů'); ('V', 'V'); ('W', 'Ŵ'); ('X', 'X'); ('Y', 'Ÿ'); ('Z', 'Ż'); ('a', 'ä'); ('b', 'þ'); ('c', 'č'); ('d', 'đ');
        ('e', 'ę'); ('f', 'ƒ'); ('g', 'ģ'); ('h', 'ĥ'); ('i', 'į'); ('j', 'ĵ'); ('k', 'ĸ'); ('l', 'ľ'); ('m', 'm'); ('n', 'ŉ');
        ('o', 'ő'); ('p', 'p'); ('q', 'q'); ('r', 'ř'); ('s', 'ş'); ('t', 'ŧ'); ('u', 'ū'); ('v', 'v'); ('w', 'ŵ'); ('x', 'χ');
        ('y', 'y'); ('z', 'ž');
    ] |> Map.ofList

//Single case active pattern (used in later pattern match)
let (|StartsWith|) (value:string) starter = value.StartsWith(starter)

//The convert function.  (Type annotation.  Mutable.  Range expression.)
let convertToFakeInternationalized (inputString:string) =
    let OrigLen = inputString.Length
    let PseudoLen = 
        if OrigLen < 10 then (OrigLen * 4) + OrigLen
        else ((int)((float OrigLen) * 0.3)) + OrigLen

    //THIS IS WHAT FP DOES TO YOU!  THIS IS JUST FOR FUN.
    let appendC, appendS, toString =
        //Make our string builder...
        let sb = new System.Text.StringBuilder(PseudoLen)
        //Return functions but not the SB.  Can't touch this!
        (fun (c:char) -> sb.Append(c) |> ignore),
        (fun (s:string) -> sb.Append(s) |> ignore),
        (fun () -> sb.ToString())

    appendC '['

    let mutable waitingForEndBrace = false
    let mutable waitingForGreaterThan = false

    for currChar in inputString do
        match currChar with
        | '{' -> waitingForEndBrace <- true
        | '}' -> waitingForEndBrace <- false
        | '<' -> waitingForGreaterThan <- true
        | '>' -> waitingForGreaterThan <- false
        | _ -> ()
        if waitingForEndBrace || waitingForGreaterThan then
            appendC currChar
        else
            let maybeChar = charMap.TryFind currChar
            match maybeChar with
            | None -> appendC currChar
            | Some(fakeChar) -> appendC fakeChar
            ()

    // Poke on extra text to fill out the string.
    let PadStr = " !!!" ;
    let PadCount = 
        let pc = (PseudoLen - OrigLen - 2) / PadStr.Length 
        if pc < 2 then 2 else pc

    for i in 1 .. PadCount do appendS PadStr

    appendC ']'
    toString()

open System.Resources
open System.Collections
 
[<EntryPoint>]
let main argv = 
    
    //Local function definition.
    let wl (msg:string) = System.Console.WriteLine(msg)

    wl "Psuedoizer: Adapted from MSDN BugSlayer 2004-Apr i18n Article."

    if argv.Length <> 2 then
        wl usageMsg
        int ExitCode.BadArgCount
    else
        let fileName, fileNameOut = argv.[0], argv.[1]

        try
            //Pipeline operator and value scoping...
            let resources = 
                new ResXResourceReader(fileName)
                |> Seq.cast<DictionaryEntry>

            //Pick the resources to convert.  (Seq expression, pattern match, single case active pattern).
            let kvps = seq { 
                for resource in resources do
                    match resource.Value with
                    | :? System.String as stringValue -> 
                        let key = resource.Key.ToString()
                        match key with
                        | "" -> ()
                        | StartsWith ">>" true | StartsWith "$" true -> ()
                        | "$this.Text" | _ -> yield key, resource.Value.ToString()
                    | _ -> ()
                }

            //Convert to fake internationalized. (tuple syntax, pipeline, shadowing)
            let convertedKvps = 
                kvps
                |> Seq.map (fun (key, value) -> key, convertToFakeInternationalized value)
                |> List.ofSeq

            //Write the new values 
            match convertedKvps with
            | [] -> 
                wl <| sprintf "WARNING: No text resources found in %s." fileName
                int ExitCode.NoTextResourcesFound
            | resources ->
                use writer = new ResXResourceWriter(fileNameOut)
                for key, value in resources do
                    writer.AddResource(key, value)
                writer.Generate()
                wl <| sprintf "Converted %i text resource(s)." resources.Length
                int ExitCode.OK
        with
        | ex ->
            wl (ex.ToString())
            int ExitCode.InFileNameNotResx