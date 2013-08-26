module Integer

// the problem is that for some pairs of natural numbers, 
//  Subtract n1 n2 
// doesn't make any sense

// the easiest way to solve that is to introduce an object 
// that's a Pair of Natural numbers, 
// where (One,Two) represents "the result of subtracting Two from One"

type Pair = Natural.Natural * Natural.Natural

// can have different pairs that represent the same number
// for instance, want (Three,Two) = (Two,One) = [the integer corresponding to One]
// since Add Two One = Three, Add One One = Two, and so on

// need to establish equivalence classes
type Integer =
| Positive of Natural.Natural
| Zero
| Negative of Natural.Natural

// map a PairInteger to its equivalence class
let MakeInteger (plus,minus) =
    match Natural.Compare plus minus with
    | Comparison.Equal -> Zero
    | Comparison.GreaterThan -> Positive (Natural.Subtract plus minus)
    | Comparison.LessThan -> Negative (Natural.Subtract minus plus)

let SuccessorOf (i : Integer) =
    match i with
    | Positive n -> Positive (Natural.SuccessorOf n)
    | Zero -> Positive (Natural.One)
    | Negative n ->
        match n with
        | Natural.One -> Zero
        | Natural.SuccessorOf n' -> Negative n'

// given an integer, get back an (arbitrary) pair that leads to it
let ExtractPair (i : Integer) =
    match i with
    | Positive c -> (Natural.SuccessorOf c, Natural.One)
    | Zero -> (Natural.One, Natural.One)
    | Negative c -> (Natural.One, Natural.SuccessorOf c)

let SuccessorOf2 (i : Integer) =
    let pos, neg = ExtractPair i
    MakeInteger (Natural.SuccessorOf pos, neg)

let One = SuccessorOf Zero
let Two = SuccessorOf One
let Three = SuccessorOf Two
// .. and so on

let Negate (i : Integer) = 
    match i with
    | Positive c -> Negative c
    | Zero -> Zero
    | Negative c -> Positive c

let PredecessorOf = Negate >> SuccessorOf >> Negate
    
let MinusOne = PredecessorOf Zero
let MinusTwo = PredecessorOf MinusOne
// .. and so on

let rec Add (i1 : Integer) (i2 : Integer) =
    match i2 with
    | Zero -> i1
    | Positive _ -> Add (SuccessorOf i1) (PredecessorOf i2)
    | Negative _ -> Add (PredecessorOf i1) (SuccessorOf i2)

let EqualTo (i1 : Integer) (i2 : Integer) =
    match i1, i2 with
    | Positive n1, Positive n2 -> Natural.EqualTo n1 n2
    | Negative n1, Negative n2 -> Natural.EqualTo n1 n2
    | Zero, Zero -> true
    | _ -> false

let rec EqualTo' (i1 : Integer) (i2 : Integer) = 
    match i1 with
    | Zero -> i2 = Zero
    | Positive _ -> EqualTo' (PredecessorOf i1) (PredecessorOf i2)
    | Negative _ -> EqualTo' (SuccessorOf i1) (SuccessorOf i2)

let LessThan (i1 : Integer) (i2 : Integer) =
    match i1, i2 with
    | Negative _, Positive _ -> true
    | Negative _, Zero -> true
    | Zero, Positive _ -> true
    | Positive n1, Positive n2 -> Natural.LessThan n1 n2
    | Negative n1, Negative n2 -> Natural.LessThan n2 n1
    | _ -> false

let Compare (i1 : Integer) (i2 : Integer) =
    if EqualTo i1 i2 then Comparison.Equal
    elif LessThan i1 i2 then Comparison.LessThan
    else Comparison.GreaterThan

let Max (i1 : Integer) (i2 : Integer) = if LessThan i1 i2 then i2 else i1
let Min (i1 : Integer) (i2 : Integer) = if LessThan i1 i2 then i1 else i2

let LessThanOrEqualTo i1 i2 = Comparison.GreaterThan <> Compare i1 i2

let rec Subtract (i1 : Integer) (i2 : Integer) =
    match i2 with
    | Zero -> i1
    // otherwise recur moving i1 toward Zero
    | Positive _ -> Subtract (PredecessorOf i1) (PredecessorOf i2)
    | Negative _ -> Subtract (SuccessorOf i1) (SuccessorOf i2)

let rec Multiply (i1 : Integer) (i2 : Integer) =
    match i2 with
    | Zero -> Zero
    | Negative _ -> Multiply (Negate i1) (Negate i2)
    | Positive _ -> Add i1 (Multiply i1 (PredecessorOf i2))

let Square (i : Integer) = Multiply i i

let AbsoluteValue (i : Integer) =
    match i with
    | Negative c -> Positive c
    | _ -> i

let rec TryDivide (i1 : Integer) (i2 : Integer) =
    match i1, i2 with
    | _, Zero -> failwithf "Division by Zero is not allowed"
    | _, Negative _ -> TryDivide (Negate i1) (Negate i2)
    | Zero, Positive _ -> Some Zero
    | Negative _, Positive _ -> 
        match TryDivide (Negate i1) i2 with
        | Some i -> Some (Negate i)
        | None -> None
    | Positive _ , Positive _ ->
        if LessThan i1 i2
        then None // cannot divide a smaller integer by a larger one
        else 
            match TryDivide (Subtract i1 i2) i2 with
            | Some i -> Some (SuccessorOf i)
            | None -> None

let rec Divide (i1 : Integer) (i2 : Integer) =
    match TryDivide i1 i2 with
    | Some i -> i
    | None -> failwithf "cannot divide a smaller integer by a larger one"

let rec GCD (i1 : Integer) (i2 : Integer) =
    // Euclidean Algorithm for computing GCD
    match i1, i2 with
    // want GCD to always be positive, so get rid of negative signs
    | Negative _, _ -> GCD (Negate i1) i2
    | _, Negative _ -> GCD i1 (Negate i2)
    | Zero, _ -> i2
    | _, Zero -> i1
    | Positive c1, Positive c2 when Natural.LessThanOrEqualTo c1 c2 -> GCD i1 (Subtract i2 i1)
    | Positive _, Positive _ -> GCD (Subtract i1 i2) i2 

let ToCounting (i : Integer) =
    match i with
    | Positive c -> c
    | _ -> failwithf "Can only convert positive Integer to counting numbers"

let rec Modulo (i : Integer) (b : Integer) =
    if LessThan Zero b
    then
        match i with
        | Zero -> Zero
        | Negative _ -> Modulo (Add i b) b
        | Positive _ -> if LessThan i b then i else Modulo (Subtract i b) b
    else failwithf "Can only mod by a positive number"

let IsDivisibleBy (i1 : Integer) (i2 : Integer) =
    EqualTo Zero (Modulo i1 (AbsoluteValue i2))

let rec IsEven (i : Integer) =
    match i with
    | Zero -> true
    | Negative _ -> IsEven (Negate i)
    | _ -> not (IsEven (PredecessorOf i))

let IsOdd = not << IsEven

let rec Range (lo : Integer) (hi : Integer) =
    if LessThan hi lo then []
    else lo :: (Range (SuccessorOf lo) hi)

let AlmostSquareRoot (i : Integer) =
    // if i is non-negative, find the largest i' with i' * i' <= i
    match i with 
    | Zero -> Zero
    | Negative _ -> failwithf "Cannot take square root of a negative number"
    | Positive _ ->
        let rec loop (i' : Integer) =
            let nexti' = SuccessorOf i'
            if LessThan i (Square nexti')
            then i'
            else loop nexti'
        loop One

let rec IsPrime (i : Integer) =
    match i with
    | Zero -> false
    | Negative _ -> IsPrime (Negate i)
    | Positive Natural.One -> false
    | Positive _ ->
        let isComposite =
            Range Two (AlmostSquareRoot i)
            |> Seq.exists (fun i' -> IsDivisibleBy i i')
        not isComposite 

let Factorial (i : Integer) =
    match i with
    | Negative _ -> failwithf "Cannot factorial a negative number"
    | Zero -> One
    | Positive _ -> Range One i |> Seq.reduce Multiply

let AllIntegers =
    Seq.append 
        [ Zero ]
        (Seq.collect (fun c -> [Negative c; Positive c]) Natural.AllNaturals)

let AllPrimes =
    Natural.AllNaturals
    |> Seq.map Positive
    |> Seq.filter IsPrime