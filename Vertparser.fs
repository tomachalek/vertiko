namespace Vertiko

module Vertparser =

    open System.IO
    open System.Text.RegularExpressions

    type Attribute = {Name: string}

    type Structure = {Name: string; Attrs: Attribute []}


    let readFile (path:string) = seq {
        use sr = new StreamReader(path)
        while not sr.EndOfStream do 
            yield sr.ReadLine()
    }

    let parseWord w = 
        Regex.Split(w, @"\s+")

    let parseTag t =
        let mc = Regex.Matches(t, @"(\w+)=""([^""]+)""")
        mc 
            |> Seq.cast 
            |> Seq.map (fun (m:Match) -> (m.Groups.Item(1).Value, m.Groups.Item(2).Value)) 
            |> Map.ofSeq

    let getTagName t : string Option =
        let m = Regex.Match(t, @"</?([\w]+)")
        match m.Success with
        | true -> Some(m.Groups.Item(1).Value)
        | false -> None

    let isStartTag (t:string) : bool =
        t.StartsWith "<" || t.StartsWith "</" && not (t.EndsWith "/>") && t.EndsWith ">"

    let isEndTag (t:string) : bool =
        t.StartsWith "</" && t.EndsWith ">"

    let isSelfClosingTag (t:string) : bool =
        t.StartsWith("<") && not (t.StartsWith "</") && t.EndsWith "/>"

    let parseLine (line:string) =
        match line with
        | x when isStartTag x -> (getTagName x, true, parseTag x)
        | x when isEndTag x -> (getTagName x, false, Map.empty)
        | x when isSelfClosingTag x -> (Some "#SELF", false, Map.empty)
        | _ -> (Some "#TEXT", false, Map.empty)
    

