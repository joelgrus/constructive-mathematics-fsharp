module Reals

// following http://en.wikipedia.org/wiki/Constructivism_(mathematics)#Example_from_real_analysis
// we'll define a Real numbers as a pair of functions:
// f : Integer -> Rational
// g : Integer -> Integer
// such that for any n, and for any i and j >= g(n) we have
//  AbsoluteValue (Subtract (f i) (f j)) <= Invert n

type IntegerToRational = Integer.Integer -> Rational.Rational
type IntegerToInteger = Integer.Integer -> Integer.Integer
type Real = IntegerToRational * IntegerToInteger

let Constantly (q : Rational.Rational) (_ : Integer.Integer) = q
let AlwaysOne (_ : Integer.Integer) = Integer.One
let FromRational (q : Rational.Rational) : Real = (Constantly q), AlwaysOne

let Zero = FromRational Rational.Zero
let One = FromRational Rational.One

// per Wikipedia, this is one way to represent the number E
let E = 
    // helper to compute partial sums of E
    let partialSumTo (n : Integer.Integer) =
        Integer.Range Integer.Zero n
        |> Seq.map (Integer.Factorial >> Rational.FromInteger >> Rational.Invert)
        |> Seq.reduce Rational.Add
    partialSumTo, id

// there's no definitive way to determine whether real numbers are the same 
// in a finite number of steps (hence in code)
// for instance, 
//  r = (fun n -> MakeRational n Integer.one) AlwaysOne
// represents the same Real number as Zero defined above,
// but there's no way to know that other than reasoning outside the system
// what we CAN say is that the two are equal *within a certain tolerance*,
// for instance, we can definitively say that r and Zero are within 1/1000 of each other

let TOLERANCE = 
    // we'll use 1/1024
    let ten = Integer.Add Integer.One (Integer.Multiply Integer.Three Integer.Three)
    let oneHalf = Rational.MakeRational Integer.One Integer.Two
    Rational.Power oneHalf ten

// compare two real numbers, returning equal if they're within a "tolerance" of each other
let CompareWithTolerance (tolerance : Rational.Rational) ((f1,g1) : Real) ((f2,g2) : Real) =
    // find first n such that 1 / n is less than tolerance'
    let n, nInverse = 
        Natural.AllNaturals
        |> Seq.map Integer.Positive
        |> Seq.choose (fun i ->
            let q = Rational.MakeRational Integer.One i
            if Rational.LessThan q tolerance
            then Some(i,q)
            else None)
        |> Seq.head
    let N1 = g1 n // if n' and n'' >= N1 then | (f1 n) - (f1 n') | < tolerance
    let N2 = g2 n // if n' and n'' >= N2 then | (f2 n) - (f2 n') | < tolerance
    let q1 = f1 N1 // q1 is approximately r1
    let q2 = f2 N2 // q2 is approximately r2
    let max1 = Rational.Add q1 tolerance 
    let min1 = Rational.Subtract q1 tolerance
        // for n >= N1 we have min1 < (f1 n) < max1
    let max2 = Rational.Add q2 tolerance
    let min2 = Rational.Subtract q2 tolerance
        // for n >= N2 we have min2 < (f2 n) < max2
    if Rational.LessThan tolerance (Rational.Subtract min2 max1)
    then
        // for n > Max(N1,N2) we have
        //    (f2 n) - (f1 n) > min2 - max1 > tolerance
        Comparison.LessThan
    elif Rational.LessThan tolerance (Rational.Subtract min1 max2) 
    then 
        // for n > Max(N1,N2) we have
        //    (f1 n) - (f2 n) > min1 - max2 > tolerance
        Comparison.GreaterThan
    else
        // otherwise too close to say 
        Comparison.Equal

let Compare = CompareWithTolerance TOLERANCE

let Negate ((f,g) : Real) = (f >> Rational.Negate), g

let Add ((f1,g1) : Real) ((f2,g2) : Real) =
    // the sum of the series converges
    let f n = Rational.Add (f1 n) (f2 n)
    // |f n' - f n''| is at most twice the max of | f1 n' - f1 n'' | and | f2 n' - f2 n'' |
    // in particular, if we set
    let g (n : Integer.Integer) = 
        let twoN = Integer.Multiply Integer.Two n
        Integer.Max (g1 twoN) (g2 twoN)
    // then we have f n' and f n'' are within 1 / n whenever n', n'' >= n
    f,g

let Subtract (r1 : Real) (r2 : Real) = Add r1 (Negate r2)

let Multiply ((f1,g1) : Real) ((f2,g2) : Real) =
    // the product of the series converges
    let f n = Rational.Multiply (f1 n) (f2 n)
    // then f n' - f n'' = (f1 n') * (f1 n'') - (f2 n') (f2 n'')
    //  = (f1 n') (f2 n' - f2 n'') + (f2 n'') (f1 n' - f1 n'')
    // <= 2 * Max( f1 n' (f2 n' - f2 n''), f2 n'' (f1 n' - f1 n''))
    //
    // in particular, if n', n'' >= g1(1), then f1 n' is within 1 of f1 n''
    // in particular particular, ABS(f1 n') <= ABS(f1 (g1 1)) + 1
    // similarly, ABS(f2 n') <= ABS(f2 (g2 1)) + 1 if n' >= (g2 1)
    let g1of1 = g1 Integer.One
    let g2of1 = g2 Integer.One
    let gof1 = Integer.Max g1of1 g2of1
    let f1bound = Rational.Add (Rational.AbsoluteValue (f1 g1of1)) Rational.One
    let f2bound = Rational.Add (Rational.AbsoluteValue (f2 g2of1)) Rational.One
    let K = Rational.RoundUp (Rational.Max f1bound f2bound)
    let g (n : Integer.Integer) =
        let N = [ Integer.Two ; n ; K ] |> Seq.reduce Integer.Multiply
        let g1n = g1 N // n' and n'' must be this big for f1 n' - f1 n'' to be less than 1 / 2Kn
        let g2n = g2 N // n' and n'' must be this big for f2 n' - f2 n'' to be less than 1 / 2Kn
        
        [g1of1; g2of1; g1n; g2n] |> Seq.reduce Integer.Max
    f, g


let TryInvertWithTolerance (tolerance : Rational.Rational) ((f,g) : Real) =
    match CompareWithTolerance tolerance (f,g) Zero with
    | Comparison.Equal -> None
    | _ -> 
        // otherwise, we know that Abs(r) > tolerance
        // | 1 / (f n) - 1 / (f n') |
        // = | (f n') - (f n) | / | (f n)(f n') |
        // <= | (f n') - (f n) | / (tolerance ^ 2)
        // when n, n' is big enough

        let N1 = tolerance |> Rational.Invert |> Rational.RoundUp |> Integer.Square |> g
            // if n, n' >= N1, then | (f n) - (f n') | < (tolerance)^2
        
        let fInverted n =
            let fn = f n
            if Rational.EqualTo Rational.Zero fn
            then Rational.One
            else Rational.Invert fn

        let gInverted n = Integer.Max (g n) N1

        Some (fInverted, gInverted)

let TryInvert = TryInvertWithTolerance TOLERANCE

let Invert (r : Real) =
    match TryInvert r with
    | Some(oneOverR) -> oneOverR
    | None -> failwithf "unable to invert a number so close to zero"
    
let TryDivide (r1 : Real) (r2 : Real) =
    match TryInvert r2 with
    | Some(oneOverR) -> Some(Multiply r1 oneOverR)
    | None -> None

let Divide r1 r2 =
    match TryDivide r1 r2 with
    | Some(r) -> r
    | None -> failwithf "unable to divide by a number so close to zero"

