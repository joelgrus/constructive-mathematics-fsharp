namespace NaturalTests

open NUnit.Framework
open FsUnit
open Natural

// some Helper methods that use .NET Int32
module TestHelpers =

    let rec ToInt32 n =
        match n with
        | Natural.One -> 1
        | Natural.SuccessorOf n' -> 1 + ToInt32 n'

    let ToString (c : Natural) =
        (ToInt32 c).ToString()

    let rec FromInt32 int32 = 
        if int32 <= 0
        then failwithf "counting number must be > 0" 
        else 
            [1 .. int32 - 1] 
            |> Seq.fold (fun n _ -> SuccessorOf n) One


[<TestFixture>]
type ``EqualTo Should Give The Right Results`` ()=

    [<Test>] 
    member x.``One should equal One`` ()=
        EqualTo One One |> should be True

    [<Test>] 
    member x.``Three should equal Three`` ()=
        EqualTo Three Three |> should be True

    [<Test>] 
    member x.``One should not equal Three`` ()=
        EqualTo One Three |> should be False

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

[<TestFixture>]
type ``LessThanOrEqualTo Should Give the Right Results`` ()=
    
    [<Test>]
    member x.``One should be less than or equal to One`` ()=
        LessThanOrEqualTo One One |> should be True

    [<Test>]
    member x.``One should be less than or equal to Three`` ()=
        LessThanOrEqualTo One Three |> should be True

    [<Test>]
    member x.``Three should not be less than or equal to One`` ()=
        LessThanOrEqualTo Three One |> should be False

[<TestFixture>]
type ``Add Should Give the Right Results`` ()=

    [<Test>]
    member x.``One plus One should equal Two`` ()=
        EqualTo Two (Add One One) |> should be True

    [<Test>]
    member x.``One plus Two should equal Three`` ()=
        EqualTo Three (Add One Two) |> should be True

    [<Test>]
    member x.``Two plus One should equal Three`` ()=
        EqualTo Three (Add Two One) |> should be True

    [<Test>]
    member x.``100 + 50 should equal 150`` () =
        let fifty = TestHelpers.FromInt32 50
        let oneHundred = TestHelpers.FromInt32 100
        let oneHundredFifty = TestHelpers.FromInt32 150
        EqualTo oneHundredFifty (Add fifty oneHundred) |> should be True

[<TestFixture>]
type ``Multiply Should Give the Right Results`` ()=

    [<Test>]
    member x.``One times One should equal One`` ()=
        EqualTo One (Multiply One One) |> should be True

    [<Test>]
    member x.``Two times Two should equal Four`` ()=
        EqualTo Four (Multiply Two Two) |> should be True

    [<Test>]
    member x.``6 * 7 should be 42`` ()=
        let six = TestHelpers.FromInt32 6
        let seven = TestHelpers.FromInt32 7
        let fortyTwo = TestHelpers.FromInt32 42
        EqualTo fortyTwo (Multiply six seven) |> should be True

[<TestFixture>]
type ``AllNaturals Contains All Naturals`` ()=

    [<Test>]
    member x.``No number appears twice in the first 1000`` ()=
        let first1000 = AllNaturals |> Seq.take 1000 |> Seq.toArray
        [|0 .. 999|]
        |> Seq.forall (fun i ->
            let ith = first1000.[i]
            first1000 |> Seq.filter (fun n -> EqualTo n ith) |> Seq.length = 1)
        |> should be True

    [<Test>]
    member x.``The number 57 appears in the list`` ()=
        let fiftySeven = TestHelpers.FromInt32 57
        AllNaturals
        |> Seq.exists (fun n -> EqualTo n fiftySeven)
        |> should be True

[<TestFixture>]
type ``TrySubtract Should Give the Right Results`` ()=

    [<Test>]
    member x.``TrySubtract One One should be None`` ()=
        TrySubtract One One |> Option.isSome |> should be False

    [<Test>]
    member x.``TrySubtract Two Three should be None`` ()=
        TrySubtract Two Three |> Option.isSome |> should be False

    [<Test>]
    member x.``TrySubtract Three One should be Some Two`` ()=
        match TrySubtract Three One with
        | Some n -> EqualTo n Two
        | None -> false
        |> should be True

    [<Test>]
    member x.``TrySubtract 100 50 should be Some 50`` () =
        let fifty = TestHelpers.FromInt32 50
        let oneHundred = TestHelpers.FromInt32 100
        match TrySubtract oneHundred fifty with
        | Some n -> EqualTo n fifty
        | None -> false
        |> should be True
