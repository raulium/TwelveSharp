﻿let nseq n from =
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

let persistence x =
  let mutable v = x
  let mutable rem = System.Numerics.BigInteger.One
  let zero = System.Numerics.BigInteger.Zero
  let ten  = System.Numerics.BigInteger(10)
  let mutable p = 0
  while v >= ten do
    let mutable t = System.Numerics.BigInteger.One
    while v > zero && t > zero do
      v <- System.Numerics.BigInteger.DivRem(v,ten, &rem)
      t <- t * rem
    v <- t
    p <- p + 1
    //eprintfn "v: %A; t: %A; rem: %A" v t rem
  p
let inline persistenceStr x = System.Numerics.BigInteger.Parse(x) |> persistence

let fastseq n =
  seq{
    let a = System.String('2',n)
    let b = System.String('3',n)
    let c = System.String('5',n)
    let d = System.String('7',n)
    for a' = n downto 0 do
      for b' = n downto 0 do
        for c' = n downto 0 do
          for d' = n downto 0 do
            let t = a.Substring(a') + b.Substring(b') + c.Substring(c') + d.Substring(d')
            if t <> "" then yield t
  }

let fasterseq n =
  let inline pow x y = System.Numerics.BigInteger.Pow(x,y)
  seq{
    let a = System.Numerics.BigInteger(2)
    let b = System.Numerics.BigInteger(3)
    let c = System.Numerics.BigInteger(5)
    let d = System.Numerics.BigInteger(7)
    for a' = 0 to n do
      for b' = 0 to n do
        for c' = 0 to n do
          for d' = 0 to n do
            yield (pow d a') * (pow c b') * (pow b c') * (pow a d')
            
  }

let fastersseq n =
  let inline initialize n v  =
    seq{
      let mutable result = System.Numerics.BigInteger.One
      yield result
      for _i = 1 to n do
        result <- result * v
        yield result
    } 
  seq{
    let two = System.Numerics.BigInteger(2)
    let three = System.Numerics.BigInteger(3)
    for a' in System.Numerics.BigInteger(6) |> initialize n do
      for b' in System.Numerics.BigInteger(7) |> initialize n do
        let ab = a' * b'
        for c' in System.Numerics.BigInteger(8) |> initialize n do
          let abc = ab * c'
          for d' in System.Numerics.BigInteger(9) |> initialize n do
            let abcd =  abc * d'
            yield abcd
            yield abcd * two
            yield abcd * three
            
  }

let inline closureseq n callback =
  let inline initialize n v  =
      seq{
        let mutable result = System.Numerics.BigInteger.One
        yield result
        for _i = 1 to n do
          result <- result * v
          yield result
      } |> Array.ofSeq
  
  ()
  |>
  fun () ->
    let two = System.Numerics.BigInteger(2)
    let three = System.Numerics.BigInteger(3)
    for a' in System.Numerics.BigInteger(6) |> initialize n do
    for b' in System.Numerics.BigInteger(7) |> initialize n do
      let ab = a' * b'
      for c' in System.Numerics.BigInteger(8) |> initialize n do
      let abc = ab * c'
      for d' in System.Numerics.BigInteger(9) |> initialize n do
        let abcd =  abc * d'
        callback <| abcd
        callback <|abcd * two
        callback <| abcd * three

let inline closureexactly n callback =
  let inline initialize n v  =
      seq{
        let mutable result = System.Numerics.BigInteger.One
        yield result
        for _i = 1 to n do
          result <- result * v
          yield result
      } |> Array.ofSeq
  
  
  let two = System.Numerics.BigInteger(2)
  let three = System.Numerics.BigInteger(3)
  let av = System.Numerics.BigInteger(6) |> initialize n
  let bv = System.Numerics.BigInteger(7) |> initialize n
  let cv = System.Numerics.BigInteger(8) |> initialize n
  let dv = System.Numerics.BigInteger(9) |> initialize n
  for ai = 0 to n do
    for bi = 0 to n - ai do
      let ab = av.[ai] * bv.[bi]
      for ci = 0 to n - (ai + bi) do
        let abc = ab * cv.[ci]
        let di =  n - (ai + bi + ci) 
        let abcd =  abc * dv.[di]
        callback <| abcd
        callback <|abcd * two
        callback <| abcd * three

let testAt n = 
  let s = new System.Diagnostics.Stopwatch()
  do s.Start()
  let results = Array.init 256 (fun _ -> 0UL)
  let inline go x = let p = persistence x in results.[p] <- results.[p] + 1UL
  do closureexactly n go
  let max = results |> Array.mapi (fun i j -> i,j) |> Array.filter (fun (i,j) -> j > 0UL)
  s.Stop()
  sprintf "n=%i the persistence spectriume is %s; time %ims" n (max |> Seq.map (sprintf "%A") |> String.concat "; ")  s.ElapsedMilliseconds
  


let isInt (x: string) = let s,_ = System.Int32.TryParse(x) in s

[<EntryPoint>]
let main =
  function
  | [|n; heads; from|] when isInt n  ->
    let t = rseq (System.Int32.Parse(n)) heads from
    t |> Seq.iter (printf "%s; ")
    Seq.length t |> printfn "\nlength : %i" 
    let tst = isSorted t |> Array.ofSeq
    if tst.Length = 0 then printfn "it is sorted." else printfn "Counter examples: %A" tst
    0 // return an integer exit code
  | [|"test"; n;|] when isInt n -> // heads; from|] ->
    let heads = "23"
    let from  = "6789"
    let s = new System.Diagnostics.Stopwatch()
    do s.Start()
    let n = (System.Int32.Parse(n))
    printfn "n is %i" n
    let t = rseq n heads from
    let max = t |> Seq.countBy persistenceStr |> Seq.sortBy fst |> Array.ofSeq
    s.Stop()
    printfn "test the persistence spectriume is %s; time %ims" (max |> Seq.map (sprintf "%A") |> String.concat "; ")  s.ElapsedMilliseconds
    0
  | [|"ftest"; n|] when isInt n ->
    let s = new System.Diagnostics.Stopwatch()
    do s.Start()
    let n = (System.Int32.Parse(n))
    printfn "n is %i" n
    let t = fastseq n
    let max = t |> Seq.countBy persistenceStr |> Seq.sortBy fst |> Array.ofSeq
    s.Stop()
    printfn "ftest the persistence spectriume is %s; time %ims" (max |> Seq.map (sprintf "%A") |> String.concat "; ")  s.ElapsedMilliseconds
    0
  | [|"ntest"; n|] when isInt n ->
    let s = new System.Diagnostics.Stopwatch()
    do s.Start()
    let n = (System.Int32.Parse(n))
    printfn "n is %i" n
    let t = fasterseq n
    let max = t |> Seq.countBy persistence |> Seq.sortBy fst |> Array.ofSeq
    s.Stop()
    printfn "ntest the persistence spectriume is %s; time %ims" (max |> Seq.map (sprintf "%A") |> String.concat "; ")  s.ElapsedMilliseconds
    0
  | [|"nrtest"; n|] when isInt n ->
    let s = new System.Diagnostics.Stopwatch()
    do s.Start()
    let n = (System.Int32.Parse(n))
    printfn "n is %i" n
    let t = fastersseq n
    let max = t |> Seq.countBy persistence |> Seq.sortBy fst |> Array.ofSeq
    s.Stop()
    printfn "ntest the persistence spectriume is %s; time %ims" (max |> Seq.map (sprintf "%A") |> String.concat "; ")  s.ElapsedMilliseconds
    0
  | [|"cbtest"; n|] when isInt n ->
    let s = new System.Diagnostics.Stopwatch()
    do s.Start()
    let n = (System.Int32.Parse(n))
    printfn "n is %i" n
    let results = Array.init 256 (fun _ -> 0UL)
    let inline go x = let p = persistence x in results.[p] <- results.[p] + 1UL
    do closureseq n go
    let max = results |> Array.mapi (fun i j -> i,j) |> Array.filter (fun (i,j) -> j > 0UL)
    s.Stop()
    printfn "ntest the persistence spectriume is %s; time %ims" (max |> Seq.map (sprintf "%A") |> String.concat "; ")  s.ElapsedMilliseconds
    0
  | [|"cetest"; n|] when isInt n ->
    let n = (System.Int32.Parse(n))
    testAt n |> System.Console.WriteLine
    0
  | [|"ptest"; n; m|] when (isInt n) && (isInt m) ->
    let n = System.Int32.Parse(n)
    let m = System.Int32.Parse(m)
    let v = Array.init (m-n) (fun i -> n + i)
    v
    |> Array.Parallel.map testAt
    |> Array.iter (printfn "%s")
    0
  | _ ->
    eprintfn "usage : \n\ttest <n>\n\tftest <n>\n\tntest <n>\n\tnrtest <n>\n\tcbtest <n>\n\tcetest <n>\n\tptest <n> <m>"
    1 
