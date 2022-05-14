// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun _ -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s -> Success((), {s with vars = s.vars.Tail}))

    let wordLength : SM<int> =
        S (fun s -> Success(s.word.Length, {s with vars = s.vars}))

    let characterValue (pos : int) : SM<char> =
        S(fun s -> 
        match List.tryItem pos s.word with
            | Some (c, _) -> Success (c, s)
            | None -> Failure (IndexOutOfBounds pos))

    let pointValue (pos : int) : SM<int> =
        S(fun s -> 
        match List.tryItem pos s.word with
            | Some (_, i) -> Success (i, s)
            | None -> Failure (IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms
        S (fun s -> 
              match aux s.vars with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))
    

    let declare (var : string) : SM<unit> =
        S(fun s -> 
        match var with
            | x when Set.contains x s.reserved -> Failure (ReservedName x)
            | x when Map.containsKey x s.vars.Head -> Failure (VarExists x)
            | _ when s.vars.IsEmpty-> Failure (IndexOutOfBounds 0)
            | _ -> Success((), {s with vars = Map.add var 0 s.vars.Head::s.vars.Tail}) )
        
        
    let rec remove i l =
        match i, l with
        | 0, _::xs -> xs
        | i, x::xs -> x::remove (i - 1) xs
        | _, [] -> failwith "index out of range"
        
    let rec insert i (v:Map<string, int>) l =
        match i, l with
        | 0, xs -> v::xs
        | i, x::xs -> x::insert (i - 1) v xs
        | _, [] -> failwith "index out of range"
   
    let update (x : string) (value : int) : SM<unit> =
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some _ -> Some m
                | None   -> aux ms
        S (fun s -> 
              match aux s.vars with
              | Some m -> 
                    //let newVars = remove (List.findIndex (m.Equals) s.vars) s.vars
                    Success ((),{s with vars = s.vars |> List.map (fun elem -> if elem=m then Map.add x value elem else elem)})
              | None   -> Failure (VarNotFound x))     