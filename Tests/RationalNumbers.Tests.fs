namespace RationalTests

open NUnit.Framework
open FsUnit
open Rational

module TestHelpers =
    let FromInt32s num32 denom32 =
        MakeRational (IntegerTests.TestHelpers.FromInt32 num32) (IntegerTests.TestHelpers.FromInt32 denom32)

    let ToString (r : Rational) =
        (IntegerTests.TestHelpers.ToString r.numerator) + "/" + (IntegerTests.TestHelpers.ToString r.denominator)

[<TestFixture>]
type ``LessThan Should Give the Right Results`` ()=

    [<Test>]
    member x.``2^-4 should be less than 2^-3`` ()=
        let oneHalf = TestHelpers.FromInt32s 1 2
        let tolerance =
            Power oneHalf (IntegerTests.TestHelpers.FromInt32 3)
        let check (n : int) =
            LessThan (TestHelpers.FromInt32s 1 n) tolerance
        [1 .. 100]
        |> Seq.exists (fun n ->
            check n)
        |> should be True
            
        