# Constructive Mathematics

Some people play video games for fun, I implement mathematics from scratch in F#.  Although this mostly represents me monkeying around, I suppose it's not a bad way to learn a few things about mathematics and/or F#.

## Comparison

`Comparison.fs` just contains a discriminated union that's the result of comparing two numbers.

## Natural Numbers

`NaturalNumbers.fs` recursively implements the counting numbers One, Two, Three, ...  and the associated arithmetic operations.  It's pretty much complete.  As subtraction is only sometimes valid, it's implemented as a `TrySubtract` function that returns an option type.

## Integers

Inspired by the lack of a robust 'Subtract' operation, `Integers.fs` extends the Natural Numbers to include Zero and Negatives.  It's mostly done as well (maybe it could use integer division), although its tests could be more complete.

## Rational Numbers

Similarly, as many pairs of Integers cannot be divided, `Rationals.fs` extends the Integers to include all *ratios* of Integers (except for those with Zero in the denominator).  I won't call this one complete (although I don't have in mind any additional functionality to add), and I haven't written any tests yet, so there might be bugs in it.

## Gaussian Integers

Inspired by the observation that there are no Integers whose squares are Negative, `GaussianIntegers.fs` adds an element `I` whose square is `MinusOne`.  I'm pretty sure the basic framework is correct, but it's very incomplete, and there are no tests yet.

## Real Numbers

The Rational numbers have a (huge) number of gaps in them, which is a layman's way of saying [Cauchy Sequences](http://en.wikipedia.org/wiki/Cauchy_sequence) that don't converge.  In `RealNumbers.fs` I attempt to 'fill in' these gaps.  As the real numbers are [uncountable](http://www.proofwiki.org/wiki/Real_Numbers_are_Uncountable), it's impossible to do this absolutely correctly. 

I'm trying to use some variation on the method described [here](http://en.wikipedia.org/wiki/Constructivism_(mathematics)#Example_from_real_analysis), which represents a real number as a pair `(f,g)`, where `f` is a Cauchy sequence of Rational Numbers, and `g` is a function that gives the actual cutoffs for the Cauchy bounds at 1, 1/2, 1/3, 1/4, ...  I am not convinced that this is the best way to implement real numbers, although I've made more progress than every other way I've tried.  These are still totally a work in progress and may change entirely.