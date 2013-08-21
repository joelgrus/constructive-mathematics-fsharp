namespace IntegerTests

open NUnit.Framework
open FsUnit
open Integer

module TestHelpers =

    let ToInt32 i =
        match i with
        | Integer.Positive c -> NaturalTests.TestHelpers.ToInt32 c
        | Integer.Zero -> 0
        | Integer.Negative c -> - NaturalTests.TestHelpers.ToInt32 c

    let ToString i =
        (ToInt32 i).ToString()

    let FromInt32 int32 =
        match int32 with
        | 0 -> Integer.Zero
        | x when x > 0 -> Integer.Positive (NaturalTests.TestHelpers.FromInt32 x)
        | x when x < 0 -> Integer.Negative (NaturalTests.TestHelpers.FromInt32 -x)

[<TestFixture>]
type ``EqualTo Should Give The Right Results`` ()=

    [<Test>] 
    member x.``One should equal One`` ()=
        EqualTo One One |> should be True

    [<Test>] 
    member x.``Three should equal Three`` ()=
        EqualTo Three Three |> should be True

    [<Test>] 
    member x.``Zero should equal Zero`` ()=
        EqualTo Zero Zero |> should be True

    [<Test>] 
    member x.``Minus One should equal Minus One`` ()=
        EqualTo MinusOne MinusOne |> should be True

    [<Test>] 
    member x.``One should not equal Three`` ()=
        EqualTo One Three |> should be False

    [<Test>] 
    member x.``One should not equal MinusOne`` ()=
        EqualTo One MinusOne |> should be False


[<TestFixture>]
type ``LessThan Should Give the Right Results`` ()=
    
    [<Test>]
    member x.``One should not be less than One`` ()=
        LessThan One One |> should be False

    [<Test>]
    member x.``One should be less than Three`` ()=
        LessThan One Three |> should be True

    [<Test>]
    member x.``Three should not be less than One`` ()=
        LessThan Three One |> should be False

    [<Test>]
    member x.``Minus One should be less than One`` ()=
        LessThan MinusOne One |> should be True

[<TestFixture>]
type ``Add Should Give the Right Results`` ()=

    [<Test>]
    member x.``One plus One should equal Two`` ()=
        EqualTo Two (Add One One) |> should be True

    [<Test>]
    member x.``One plus MinusOne should equal Zero`` ()=
        EqualTo Zero (Add One MinusOne) |> should be True

    [<Test>]
    member x.``Two plus Zero should equal Two`` ()=
        EqualTo Two (Add Two Zero) |> should be True

    [<Test>]
    member x.``100 + 50 should equal 150`` () =
        let fifty = TestHelpers.FromInt32 50
        let oneHundred = TestHelpers.FromInt32 100
        let oneHundredFifty = TestHelpers.FromInt32 150
        EqualTo oneHundredFifty (Add fifty oneHundred) |> should be True

    [<Test>]
    member x.``100 + -50 should equal 50`` () =
        let fifty = TestHelpers.FromInt32 50
        let oneHundred = TestHelpers.FromInt32 100
        let negativeFifty = TestHelpers.FromInt32 -50
        EqualTo fifty (Add oneHundred negativeFifty) |> should be True

[<TestFixture>]
type ``Multiply Should Give the Right Results`` ()=

    [<Test>]
    member x.``One times One should equal One`` ()=
        EqualTo One (Multiply One One) |> should be True

    [<Test>]
    member x.``Two times Two should equal Four`` ()=
        let four = TestHelpers.FromInt32 4
        EqualTo four (Multiply Two Two) |> should be True

    [<Test>]
    member x.``Two times Minus 3 should equal Minus 6`` ()=
        let minus6 = TestHelpers.FromInt32 -6
        EqualTo minus6 (Multiply Two (Negate Three)) |> should be True

    [<Test>]
    member x.``6 * 7 should be 42`` ()=
        let six = TestHelpers.FromInt32 6
        let seven = TestHelpers.FromInt32 7
        let fortyTwo = TestHelpers.FromInt32 42
        EqualTo fortyTwo (Multiply six seven) |> should be True

[<TestFixture>]
type ``AllIntegers Contains All Integers`` ()=

    [<Test>]
    member x.``No number appears twice in the first 1000`` ()=
        let first1000 = AllIntegers |> Seq.take 1000 |> Seq.toArray
        [|0 .. 999|]
        |> Seq.forall (fun i ->
            let ith = first1000.[i]
            first1000 |> Seq.filter (fun n -> EqualTo n ith) |> Seq.length = 1)
        |> should be True

    [<Test>]
    member x.``The number 57 appears in the list`` ()=
        let fiftySeven = TestHelpers.FromInt32 57
        AllIntegers
        |> Seq.exists (fun n -> EqualTo n fiftySeven)
        |> should be True

    [<Test>]
    member x.``The number -173 appears in the list`` ()=
        let minusOneSeventyThree = TestHelpers.FromInt32 -173
        AllIntegers
        |> Seq.exists (fun n -> EqualTo n minusOneSeventyThree)
        |> should be True

[<TestFixture>]
type ``Subtract Should Give the Right Results`` ()=

    [<Test>]
    member x.``Subtract One One should be Zero`` ()=
        EqualTo Zero (Subtract One One) |> should be True

    [<Test>]
    member x.``Subtract Two Three should be MinusOne`` ()=
        EqualTo MinusOne (Subtract Two Three) |> should be True

    [<Test>]
    member x.``Subtract Three One should be Two`` ()=
        EqualTo Two (Subtract Three One) |> should be True

    [<Test>]
    member x.``Subtract 100 50 should be 50`` () =
        let fifty = TestHelpers.FromInt32 50
        let oneHundred = TestHelpers.FromInt32 100
        EqualTo fifty (Subtract oneHundred fifty) |> should be True

    [<Test>]
    member x.``Subtract 50 should be -50`` () =
        let fifty = TestHelpers.FromInt32 50
        let oneHundred = TestHelpers.FromInt32 100
        EqualTo (Negate fifty) (Subtract fifty oneHundred) |> should be True
