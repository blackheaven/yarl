# yarl

Yet another records library.

Leverages `getField`

## Example

```haskell
import Data.Records.Yarl.LinkedList

type Person = Record '[Field "name" String, Field "age" Int]

marvin :: Person
marvin = Field "marvin" :> Field 42 :> RNil

desc :: Person -> String
desc p = "My name is " <> p.name <> " and I'm " <> show p.age
```

## Performance

See `bench/Main.hs`:

```
benchmarking PlainRecord.sumDirect
time                 65.36 ms   (65.06 ms .. 65.59 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 65.24 ms   (65.16 ms .. 65.35 ms)
std dev              156.1 μs   (98.87 μs .. 248.4 μs)

benchmarking PlainRecord.sumGetValueConcrete
time                 64.56 ms   (64.06 ms .. 64.95 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 65.13 ms   (64.96 ms .. 65.35 ms)
std dev              364.1 μs   (261.5 μs .. 528.7 μs)

benchmarking PlainRecord.sumGetValueClass
time                 208.7 ms   (205.5 ms .. 211.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 208.1 ms   (207.3 ms .. 208.8 ms)
std dev              1.086 ms   (783.3 μs .. 1.449 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking LinkedList.sumDirect
time                 141.1 ms   (137.7 ms .. 144.5 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 141.6 ms   (140.7 ms .. 142.7 ms)
std dev              1.435 ms   (846.2 μs .. 2.139 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking LinkedList.sumGetValueConcrete
time                 298.4 ms   (296.6 ms .. 300.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 300.4 ms   (299.5 ms .. 301.1 ms)
std dev              1.008 ms   (711.7 μs .. 1.332 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking LinkedList.sumGetValueClass
time                 3.559 s    (3.502 s .. 3.613 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.552 s    (3.543 s .. 3.561 s)
std dev              10.62 ms   (9.044 ms .. 11.69 ms)
variance introduced by outliers: 19% (moderately inflated)
```

All tests are summing up all 64 fields `Int` of a record 1 million times:

 * `sumDirect` : fetch values via pattern matching/`RecordWildCards`
 * `sumGetValueConcrete`: fetch values via `getField` type being specifing in the function definition
 * `sumGetValueClass`: fetch values via `getField` type being expressed as contraints

Globally, for a typical usage (having the type known, replacing `RecordWildCards`), you'll get a x5 (`PlainRecord.sumDirect 65ms` vs `LinkedList.sumGetValueConcrete 300 ms`), which seems reasonnable for the moment.
