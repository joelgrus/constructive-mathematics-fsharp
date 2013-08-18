namespace RationalTests

open NUnit.Framework
open FsUnit
open Rational

module TestHelpers =
    let FromInt32s num32 denom32 =
        MakeRational (IntegerTests.TestHelpers.FromInt32 num32) (IntegerTests.TestHelpers.FromInt32 denom32)

    let ToString (r : Rational) =
        (IntegerTests.TestHelpers.ToString r.numerator) + "/" + (IntegerTests.TestHelpers.ToString r.denominator)


