// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type internal MultiSet<'a when 'a: comparison> =
        { MSet: Map<'a, uint32>
          Count: uint32 }

    let empty = { MSet = Map.empty; Count = 0u }
    let isEmpty (s: MultiSet<'a>) = s.MSet.IsEmpty
    let size (s: MultiSet<'a>) = s.Count

    //let Size (s:MultiSet<_>) = Map.fold (fun acc elem -> acc + snd elem) 0u MSet
    let contains a (s: MultiSet<'a>) = s.MSet.ContainsKey a

    let numItems a (s: MultiSet<'a>) =
        if s.MSet.ContainsKey a then
            s.MSet.Item a
        else
            0u

    let add a n (s: MultiSet<'a>) =
        if s.MSet.ContainsKey a then
            { MSet = s.MSet.Add(a, s.MSet.Item a + n)
              Count = s.Count + n }
        else
            { MSet = s.MSet.Add(a, n)
              Count = s.Count + n }

    let addSingle a (s: MultiSet<'a>) = add a 1u s

    let remove a n (s: MultiSet<'a>) =
        if s.MSet.ContainsKey a
        then
            let oldAmount = s.MSet.Item a
            match oldAmount with
            | _ when s.MSet.Item a <= n -> { MSet = s.MSet.Remove(a) ; Count = s.Count - oldAmount }
            | _ -> { MSet = s.MSet.Add(a, s.MSet.Item a - n) ; Count = s.Count - n }
            else {MSet = s.MSet ; Count = s.Count}

    let removeSingle a (s: MultiSet<'a>) = remove a 1u s

    let fold f acc (s: MultiSet<'a>) = Map.fold f acc s.MSet
    let foldBack f (s: MultiSet<'a>) acc = Map.foldBack f s.MSet acc
