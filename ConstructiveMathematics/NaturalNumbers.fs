module Natural

type Natural = One | SuccessorOf of Natural

let Two = SuccessorOf One
let Three = SuccessorOf Two
let Four = SuccessorOf Three
// and so on

// given two natural numbers, either they're *equal*, or one is *greater than* the other
let rec Compare (n1 : Natural) (n2 : Natural) =
    match n1,n2 with
      // One is equal to One
    | One, One -> Comparison.Equal
      // One is less than any other natural number
    | One, SuccessorOf _ -> Comparison.LessThan
      // Any other natural number is greater than One
    | SuccessorOf _, One -> Comparison.GreaterThan
      // otherwise we compare predecessors
      // at some stage one (or both) will reach One and the recursion will stop
    | SuccessorOf n1', SuccessorOf n2' -> Compare n1' n2'

// convenience functions
let EqualTo (n1 : Natural) (n2 : Natural) = Comparison.Equal = Compare n1 n2
let LessThan (n1 : Natural) (n2 : Natural) = Comparison.LessThan = Compare n1 n2
let LessThanOrEqualTo (n1 : Natural) (n2 : Natural) = Comparison.GreaterThan <> Compare n1 n2

// now we're ready to define addition
let rec Add (n1 : Natural) (n2 : Natural) =
    match n1 with
        // adding One to a number is the same as taking its Successor
    | One -> SuccessorOf n2
        // otherwise n1 has a predecessor, add it to the successor of n2
        // idea: n1 + n2 = (n1 - 1) + (n2 + 1)
    | SuccessorOf n1' -> Add n1' (SuccessorOf n2)

let rec Multiply (n1 : Natural) (n2 : Natural) =
    match n1 with
        // similarly multiplying One by a number returns that same number
    | One -> n2
        // otherwise use idea n1 * n2 = (1 + n1 - 1) * n2 = n2 + (n1 - 1) * n2
    | SuccessorOf n1' -> Add n2 (Multiply n1' n2)

// and now we can generate a lazy infinite sequence of all the Natural numbers
// by taking Successors forever
let AllNaturals = Seq.unfold (fun c -> Some (c, SuccessorOf c)) One

// now, we'd like to define some notion of subtraction as the inverse of addition
// so if n1 + n2 = n3, then you'd like "n3 - n2" = n1
// but this isn't always defined, for instance 
//  n = One - One
// would mean One = One + n = SuccessorOf n, which plainly can never happen
// in this case we'll return None
let rec TrySubtract (n1 : Natural) (n2 : Natural) =
    match n1, n2 with
        // Since n1' + One = SucessorOf n1', then SuccessorOf n1' - One = n1'
    | SuccessorOf n1', One -> Some n1'
        // if n = (n1 + 1) - (n2 + 1), then
        //    n + n2 + 1 = n1 + 1
        // so n + n2 = n1,
        // or n = n1 - n2
    | SuccessorOf n1', SuccessorOf n2' -> TrySubtract n1' n2'
    | One, _ -> None // "Impossible subtraction"

// and if you know it's safe to do so, you can subtract
// if it's unsafe, throw an error
let Subtract (n1 : Natural) (n2 : Natural) =
    match TrySubtract n1 n2 with
    | None -> failwithf "Impossible subtraction!"
    | Some n -> n