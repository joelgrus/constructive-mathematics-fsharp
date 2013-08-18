module Rational

// motivated by the "division problem" -- given integers i1 and i2, where i2 not zero,
// would like to define some number q = Divide i1 i2, such that EqualTo i1 (Multiply q i2) 

// proceeding as above, why not define a new type of number as a *pair* (i1,i2) representing
// the "quotient" of i1 and i2.  Again such a representation is not unique, as you'd want
// (Two,One) = (Four,Two) = [the number corresponding to Two]

// when do we want (i1,i2) = (i1',i2') ?  
// when there is some i3 with i1 = i2 * i3, i1' = i2' * i3, or
// precisely when we have i1 * i2' = i1' * i2

// in particular, if x divides both i1 and i2, so that i1 = i1' * x, i2 = i2' * x, then
// i1 * i2' = i1' * x * i2' = i1' * i2, so that (i1, i2) = (i1', i2')

type Rational(numerator : Integer.Integer, denominator : Integer.Integer) =
    let gcd = 
        if Integer.EqualTo Integer.Zero denominator then failwithf "Cannot have a Zero denominator"
        else Integer.GCD numerator denominator
        
    // want denominator to be positive always
    let reSign =
        match denominator with
        | Integer.Negative _ -> Integer.Negate
        | _ -> id

    // divide by GCD to get to relatively prime
    let _numerator = (Integer.Divide (reSign numerator) gcd)
    let _denominator = (Integer.Divide (reSign denominator) gcd)

    member this.numerator with get () = _numerator
    member this.denominator with get () = _denominator

let MakeRational (numerator : Integer.Integer) (denominator : Integer.Integer) : Rational =
    new Rational(numerator,denominator)

let FromInteger (i : Integer.Integer) = MakeRational i Integer.One

let Zero = FromInteger Integer.Zero
let One = FromInteger Integer.One

let Negate (r : Rational) =
    MakeRational (Integer.Negate r.numerator) (r.denominator)

let MinusOne = Negate One

let Invert (r : Rational) =
    MakeRational r.denominator r.numerator

// if I want to (e.g.) add a/b and c/d, I first rewrite them as
// (ad / bd) and (bc / bd)
// this returns ad, bc, and bd
let InSameTerms (r1 : Rational) (r2 : Rational) =
    let commonDenominator = Integer.Multiply r1.denominator r2.denominator
    let newNumerator1 = Integer.Multiply r1.numerator r2.denominator
    let newNumerator2 = Integer.Multiply r2.numerator r1.denominator
    // want to also make sure that bd is positive
    let transform = if Integer.LessThan Integer.Zero commonDenominator then id else Integer.Negate
    (transform newNumerator1),(transform newNumerator2),(transform commonDenominator)

let EqualTo (r1 : Rational) (r2 : Rational) =
    let nn1, nn2, _ = InSameTerms r1 r2
    Integer.EqualTo nn1 nn2

let LessThan (r1 : Rational) (r2 : Rational) =
    let nn1, nn2, _ = InSameTerms r1 r2
    Integer.LessThan nn1 nn2

let Max r1 r2 = if LessThan r1 r2 then r2 else r1
let Min r1 r2 = if LessThan r1 r2 then r1 else r2

let AbsoluteValue (r : Rational) =
    if LessThan r Zero
    then Negate r
    else r

let rec Add (r1 : Rational) (r2 : Rational) =
    let nn1, nn2, cd = InSameTerms r1 r2
    MakeRational (Integer.Add nn1 nn2) cd

let rec Subtract (r1 : Rational) (r2 : Rational) =
    let nn1, nn2, cd = InSameTerms r1 r2
    MakeRational (Integer.Subtract nn1 nn2) cd

let Distance (r1 : Rational) (r2 : Rational) =
    AbsoluteValue (Subtract r1 r2)

let rec Multiply (r1 : Rational) (r2 : Rational) =
    MakeRational (Integer.Multiply r1.numerator r2.numerator) (Integer.Multiply r1.denominator r2.denominator)

let rec Divide (r1 : Rational) (r2 : Rational) =
    Multiply r1 (Invert r2)

let rec Power (r : Rational) (i : Integer.Integer) =
    match i with
    | Integer.Zero -> One
    | Integer.Negative _ -> Power (Invert r) (Integer.Negate i)
    | Integer.Positive _ -> Multiply r (Power r (Integer.PredecessorOf i))

let TryToInteger (r : Rational) = Integer.TryDivide r.numerator r.denominator
let ToInteger (r : Rational) =
    match TryToInteger r with
    | Some i -> i
    | None -> failwithf "not a rational representation of an integer"

let rec RoundUp (r : Rational) =
    if LessThan r Zero
    then Integer.Negate (RoundDown (Negate r)) 
    else
        match TryToInteger r with
        | Some i -> i
        | None ->
            Integer.Add Integer.One (RoundDown r)
and RoundDown (r : Rational) =    
    if LessThan r Zero
    then Integer.Negate (RoundUp (Negate r))
    else
        match TryToInteger r with
        | Some i -> i
        | None ->
        let n = r.numerator // necessarily >= 0
        let d = r.denominator
        let m = Integer.Modulo n d
        Integer.Divide (Integer.Subtract n m) d

let AllRationals =
    let AllForTotal (total : Natural.Natural) =
        let Total = Integer.Positive total
        Integer.Range Integer.One (Integer.PredecessorOf Total)
        |> Seq.map (fun i -> (i, Integer.Subtract Total i))
        |> Seq.filter (fun (i,j) -> Integer.EqualTo Integer.One (Integer.GCD i j))
        |> Seq.collect (fun (i,j) -> [ MakeRational i j ; MakeRational (Integer.Negate i) j ])
    Seq.append
        [ Zero ]
        (Seq.collect AllForTotal Natural.AllNaturals)
