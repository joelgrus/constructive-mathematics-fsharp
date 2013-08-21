module Gaussian

type Gaussian = Integer.Integer * Integer.Integer

let MakeGaussian r i : Gaussian = (r,i)

let Real r = MakeGaussian r Integer.Zero
let Imaginary i = MakeGaussian Integer.Zero i

let Zero = Real Integer.Zero
let One = Real Integer.One
let I = Imaginary Integer.One

let Add ((r1,i1) : Gaussian) ((r2,i2) : Gaussian) =
    MakeGaussian (Integer.Add r1 r2) (Integer.Add i1 i2)

let Multiply ((r1,i1): Gaussian) ((r2,i2) : Gaussian) =
    MakeGaussian (Integer.Subtract (Integer.Multiply r1 r2) (Integer.Multiply i1 i2))
                    (Integer.Add (Integer.Multiply i1 r2) (Integer.Multiply i2 r1))

let Norm ((r,i) : Gaussian) = Integer.Add (Integer.Square r) (Integer.Square i)

let IsUnit g = Integer.EqualTo (Norm g) Integer.One

let Conjugate ((r,i) : Gaussian) = MakeGaussian r (Integer.Negate i)

// division is a little bit harder
// idea is that (Norm g2) = g2 * Conjugate g2
// so that if g1 = g2 * g3
// then g1 * Conjugate g2 = g2 * g3 * Conjugate g2 = (Norm g2) * g3
// so that you'd need g3 = (g1 * Conjugate g2) / (Norm g2)
// which only makes sense if g1 * Conjugate g2 is divisible by Norm g2

let TryDivideByInt ((r,i) : Gaussian) int =
    match Integer.TryDivide r int, Integer.TryDivide i int with
    | Some(r'), Some(i') -> Some (MakeGaussian r' i')
    | _ -> None

let TryDivide g1 g2 = TryDivideByInt (Multiply g1 (Conjugate g2)) (Norm g2)

let Divide g1 g2 = 
    match TryDivide g1 g2 with
    | Some g -> g
    | None -> failwithf "not divisible"

let IsDivisible g1 g2 = (TryDivide g1 g2).IsSome

// distinct Gaussians whose real and imaginary parts have magnitudes m1 and m2
let rec GaussiansWithMagnitudes (m1 : Integer.Integer) (m2 : Integer.Integer) =
    match m1, m2 with
    | Integer.Zero, Integer.Zero -> [ MakeGaussian Integer.Zero Integer.Zero ]
    | Integer.Zero, _ -> [ MakeGaussian Integer.Zero m2;
                            MakeGaussian Integer.Zero (Integer.Negate m2); ]
    | _, Integer.Zero -> [ MakeGaussian m1 Integer.Zero;
                            MakeGaussian (Integer.Negate m1) Integer.Zero ]
    | _, _ -> [  MakeGaussian m1 m2;
                    MakeGaussian m1 (Integer.Negate m2);
                    MakeGaussian (Integer.Negate m1) m2;
                    MakeGaussian (Integer.Negate m1) (Integer.Negate m2) ]

let GaussianIntegersOfNorm (norm : Integer.Integer) =
    if (Integer.LessThan norm Integer.Zero) 
    then Seq.empty
    else 
        let candidates = Integer.Range Integer.Zero norm
        seq { for i1 in candidates do
                for i2 in candidates do
                    if Integer.EqualTo norm (Integer.Add (Integer.Square i1) (Integer.Square i2))
                    then
                        yield! (GaussiansWithMagnitudes i1 i2) }

let AllGaussianIntegers =
    Integer.AllIntegers
    |> Seq.filter (fun i -> Integer.LessThanOrEqualTo Integer.Zero i)
    |> Seq.collect GaussianIntegersOfNorm

let IsPrime g =
    let n = Norm g
    if Integer.LessThanOrEqualTo n Integer.One then false
    else
        Integer.Range Integer.Two (Integer.PredecessorOf n)
        |> Seq.collect GaussianIntegersOfNorm
        |> Seq.exists (fun g' -> IsDivisible g g')


