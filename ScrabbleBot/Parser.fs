// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    type squareFun = word -> int -> int -> Result<int, Error>
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy Char.IsLetterOrDigit <|> (pchar '_')  <?> "alphanumeric"
    let pAnyChar = palphanumeric <|> satisfy Char.IsWhiteSpace <|> satisfy Char.IsSymbol

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 =  p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' <?> "parenthesis"
    let curlyParenthesise p = pchar '{' >*>. p .>*> pchar '}'

    let charListToStr charList = String(List.toArray charList) |> string
    let pid = pletter <|> (pchar '_') .>*>. many palphanumeric |>> (fun (x, lst) -> charListToStr(char x::lst)) <?> "pid"
    
    let unop op a = op >*>. a <?> "unop"
    
    let binop op p1 p2 = p1 .>*> op .>*>. p2 <?> "binop"
   
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    let CAtomParse, cref = createParserForwardedToRef<cExp>()

    //Term
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref.Value <- choice [AddParse; ProdParse]
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref.Value <- choice [AddParse;SubParse; ProdParse]
    
    //Prod
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref.Value <- choice [MulParse; AtomParse]
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    do pref.Value <- choice [MulParse; DivParse; AtomParse]
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Div"
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]
    
    //Atom
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PointValue"
    let CharToIntParse = unop pCharToInt CAtomParse |>> CharToInt <?>"charToInt"
    do aref.Value <- choice [PVParse; CharToIntParse]
    let NegParse = unop (pchar '-') AtomParse |>>( fun x -> N -1 .*. x) <?> "Neg"
    do aref.Value <- choice [PVParse; CharToIntParse;NegParse]
    let NParse   = pint32 |>> N <?> "Int"
    do aref.Value <- choice [PVParse; CharToIntParse;NegParse;NParse]
    let VParse   = pid |>> V <?> "variable"
    do aref.Value <- choice [PVParse; CharToIntParse;NegParse;NParse; VParse]
    let ParParse = parenthesise TermParse <?> "parenth"
    do aref.Value <- choice [PVParse; CharToIntParse;NegParse;NParse;VParse; ParParse]
   
   
   
   
    
    //CexpParse
    let CParse = pchar ''' >>. (pAnyChar <|> whitespaceChar) .>*> pchar ''' |>> C <?> "Char"
    let CVParse = unop pCharValue AtomParse |>> CV <?> "CharValue"
    do cref.Value <- choice [CParse; CVParse]
    let IntToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "i"
    do cref.Value <- choice [CParse; CVParse; IntToCharParse]
    let ToUpperParse = unop pToUpper CAtomParse |>> ToUpper <?> "ToUpper"
    do cref.Value <- choice [CParse; CVParse; IntToCharParse; ToUpperParse]
    let ToLowerParse = unop pToLower CAtomParse |>> ToLower <?> "ToLower"
    do cref.Value <- choice [CParse; CVParse; IntToCharParse; ToUpperParse; ToLowerParse]
    let CParParse = parenthesise CAtomParse <?> "parenth"
    do cref.Value <- choice [CParse; CVParse; IntToCharParse; ToUpperParse; ToLowerParse; CParParse]
    
    
    let AexpParse = TermParse
    let CexpParse = CAtomParse

    let boolTier3Parse, b3ref = createParserForwardedToRef<bExp>()
    let boolTier2Parse, b2ref = createParserForwardedToRef<bExp>()
    let boolTier1Parse, b1ref = createParserForwardedToRef<bExp>()
    
    
    
    
    //Tier 3
    let andParse = binop (pchar '/' .>*>. pchar '\\') boolTier2Parse boolTier2Parse |>> Conj <?> "Conjunction"
    do b3ref.Value <- choice [andParse;boolTier2Parse]
    let orParse = binop (pchar '\\' .>*>. pchar '/') boolTier2Parse boolTier2Parse |>> (fun (a,b) -> a .||. b) <?> "Or"
    do b3ref.Value <- choice [andParse;orParse;boolTier2Parse]
    
     //Tier 2
    let equalsParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "equals"
    do b2ref.Value <- choice [equalsParse; boolTier1Parse]
    let altParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "less than"
    do b2ref.Value <- choice [equalsParse; altParse;boolTier1Parse]
    let notAltParse = binop (pchar '>') AexpParse AexpParse |>> (fun (a, b) -> a .>. b) <?> "bigger than"
    do b2ref.Value <- choice [equalsParse; altParse; notAltParse; boolTier1Parse]
    let inEqualParse = binop (pchar '<' .>*>. pchar '>') AexpParse AexpParse |>> (fun (a,b) -> ~~(a .=. b)) <?> "notEquals"
    do b2ref.Value <- choice [equalsParse; altParse; notAltParse; inEqualParse; boolTier1Parse]
    let altOrEqualParse = binop (pchar '<' .>*>. pchar '=') AexpParse AexpParse |>> (fun (a,b) -> a .<=. b) <?> "less than or equasl"
    do b2ref.Value <- choice [equalsParse; altParse; notAltParse; inEqualParse; altOrEqualParse; boolTier1Parse]
    let notAltOrEqualsParse = binop (pchar '>' .>*>. pchar '=') AexpParse AexpParse |>> (fun (a,b) -> a .>=. b) <?> "bigger than or equal"
    do b2ref.Value <- choice [equalsParse; altParse; notAltParse; inEqualParse; altOrEqualParse; notAltOrEqualsParse; boolTier1Parse;]
    
    //Tier 1
    let notParse = unop (pchar '~') boolTier1Parse |>> Not <?> "Not"
    let isLetterParse = unop pIsLetter CexpParse |>> IsLetter <?> "isLetter"
    do b1ref.Value <- choice [notParse; isLetterParse]
    let isVowelParse = unop pIsVowel CexpParse |>> IsVowel <?> "isVowel"
    do b1ref.Value <- choice [notParse; isLetterParse; isVowelParse;]
    let isDigitParse = unop pIsDigit CexpParse |>> IsDigit <?> "isDigit"
    do b1ref.Value <- choice [notParse; isLetterParse; isVowelParse; isDigitParse;]
    let trueParse =  pTrue |>> (fun _ -> TT) <?> "true"
    do b1ref.Value <- choice [notParse; isLetterParse; isVowelParse; isDigitParse; trueParse;]
    let falseParse = pFalse |>> (fun _ -> FF) <?> "false"
    do b1ref.Value <- choice [notParse; isLetterParse; isVowelParse; isDigitParse; trueParse;falseParse;]
    let bParParse = parenthesise boolTier3Parse <?> "parenth"
    do b1ref.Value <- choice [notParse; isLetterParse; isVowelParse; isDigitParse; trueParse;falseParse; bParParse;]
   
    let BexpParse = boolTier3Parse
    
    let stmntTopParse, strefTop = createParserForwardedToRef<stm>()
    let stmntAtomParse, stref = createParserForwardedToRef<stm>()
    
    //Statement Top parse
    let semicolonParse = binop (spaces >*>. pchar ';') stmntAtomParse stmntTopParse |>> Seq <?> "Semi Colon"
    do strefTop.Value <- choice [semicolonParse; stmntAtomParse]
    
    //Statement parse
    let assignParse = pid .>*> pchar ':' .>*> pchar '=' .>*>. AexpParse .>*> spaces  |>> Ass <?> "Ass"
    let declareParse = pdeclare .>> spaces1 >>. pid |>> Declare <?> "Declare"
    do stref.Value <- choice [assignParse; declareParse; ]
    let ifThenElseParse = pif >*>. parenthesise boolTier3Parse
                          .>*> pthen .>*>.
                          curlyParenthesise stmntTopParse .>*> pelse .>*>.
                          curlyParenthesise stmntTopParse |>> (fun (a,b) -> ITE (fst a, snd a, b)) <?> "If then else"
    do stref.Value <- choice [assignParse; declareParse; ifThenElseParse]
    let ifThenParse = pif >*>. parenthesise boolTier3Parse
                          .>*> pthen .>*>.
                          curlyParenthesise stmntTopParse |>> (fun (a,b) -> ITE (a, b, Skip)) <?> "If then"
    do stref.Value <- choice [assignParse; declareParse; ifThenElseParse; ifThenParse]
    let whileParse = pwhile >*>. parenthesise boolTier3Parse
                          .>*> pdo .>*>.
                          curlyParenthesise stmntTopParse |>>  While <?> "While"
    do stref.Value <- choice [assignParse; declareParse; ifThenElseParse; ifThenParse; whileParse]
    let stmntParse = stmntTopParse

(* These five types will move out of this file once you start working on the project *)
    type word   = (char * int) list
    type square = Map<int, squareFun>
    let parseSquareProg (sqp:squareProg) = Map.map (fun _ value -> (run stmntParse value) |> getSuccess |> stmntToSquareFun) sqp

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
        type board = {
            center        : coord
            defaultSquare : square
            squares       : boardFun2
        }
    let parseBoardProg (s:string) (sqs:Map<int, square>) :boardFun2 = stmntToBoardFun (run stmntParse s |> getSuccess) sqs
        

    let mkBoard (bp : boardProg) :board =
        let m' = Map.map (fun _ -> parseSquareProg) bp.squares
        {
        center = bp.center;
        defaultSquare = Map.find bp.usedSquare m';
        squares = parseBoardProg bp.prog m';
    }