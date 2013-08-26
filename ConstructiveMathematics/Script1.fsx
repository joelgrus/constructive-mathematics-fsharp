#load "Comparison.fs"
#load "NaturalNumbers.fs"
#load "Integers.fs"
#load "RationalNumbers.fs"
#load "RealNumbers.fs"

let rec NFromInt32 int32 = 
    if int32 <= 0
    then failwithf "counting number must be > 0" 
    else 
        [1 .. int32 - 1] 
        |> Seq.fold (fun n _ -> Natural.SuccessorOf n) Natural.One

let IFromInt32 int32 =
    match int32 with
    | 0 -> Integer.Zero
    | x when x > 0 -> Integer.Positive (NFromInt32 x)
    | x when x < 0 -> Integer.Negative (NFromInt32 -x)

let RFromInt32s num32 denom32 =
    Rational.MakeRational (IFromInt32 num32) (IFromInt32 denom32)

let rec NToInt32 n =
    match n with
    | Natural.One -> 1
    | Natural.SuccessorOf n' -> 1 + NToInt32 n'

let NToString (c : Natural.Natural) =
    (NToInt32 c).ToString()

let IToInt32 i =
    match i with
    | Integer.Positive c -> NToInt32 c
    | Integer.Zero -> 0
    | Integer.Negative c -> - NToInt32 c

let IToString i =
    (IToInt32 i).ToString()    

let RToString (r : Rational.Rational) =
    (IToString r.numerator) + "/" + (IToString r.denominator)


let oneHalf = RFromInt32s 1 2
let tolerance =
    Rational.Power oneHalf (IFromInt32 3)

let check (n : int) =
    Rational.LessThan (RFromInt32s 1 n) tolerance

[1 .. 9] |> Seq.exists check

let shouldBeTwo = Real.Multiply Real.SquareRootOfTwo Real.SquareRootOfTwo

let f,g = shouldBeTwo

4 |> IFromInt32 |> f |> RToString