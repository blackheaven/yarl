module Main where

import Criterion.Main
import PlainRecord.ScenarioDirect
import PlainRecord.ScenarioGetValueClass
import PlainRecord.ScenarioGetValueConcrete
import PlainRecord.Value

main :: IO ()
main =
  defaultMain
    [ bench "sumDirect" $ nf sumDirect oneRecord,
      bench "sumGetValueConcrete" $ nf sumGetValueConcrete oneRecord,
      bench "sumGetValueClass" $ nf sumGetValueClass oneRecord
    ]
