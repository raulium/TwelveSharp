let nseq n from =
  let rec nseq n (from :string) = 
    if n > 0 && from.Length > 0 then
      seq{
        for i = 0 to from.Length - 1 do
          let s = new System.String(from.Chars(i),n)
          for w = s.Length - 1 downto 0 do yield s.Substring(w)
          for w = 0 to s.Length - 1 do
            let prefix = s.Substring(w)
            for j = i + 1 to from.Length - 1 do
              let rest = from.Substring(j)
              for postfix in nseq n rest do
                yield prefix + postfix
      } 
    else
      "" |> Seq.singleton
  seq{let mutable last = "" in for x in nseq n from do if x > last then let _ = last <- x in yield x}

let rseq n heads from = 
  seq{
    for i = 0 to (String.length heads) - 1 do
      let prefix = string <| heads.Chars(i)
      for postfix in nseq n from do 
        yield prefix + postfix
    yield! nseq n from
  }
let isSorted x = 
  x 
  |> Seq.pairwise
  |> Seq.choose (fun (x,y) -> if x > y then Some (x,y) else None)

[<EntryPoint>]
let main =
  function
  | [|n; heads; from|] ->
    let t = rseq (System.Int32.Parse(n)) heads from
    t |> Seq.iter (printf "%s; ")
    Seq.length t |> printfn "\nlength : %i" 
    let tst = isSorted t |> Array.ofSeq
    if tst.Length = 0 then printfn "it is sorted." else printfn "Counter examples: %A" tst
    0 // return an integer exit code
  | _ ->
    eprintfn "usage : <n> <heads> <from>"
    1
