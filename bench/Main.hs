module Main where

import Control.DeepSeq
import Criterion.Main
import qualified LinkedList.ScenarioDirect as LinkedList
import qualified LinkedList.ScenarioGetValueClass as LinkedList
import qualified LinkedList.ScenarioGetValueConcrete as LinkedList
import qualified LinkedList.Value as LinkedList
import qualified PlainRecord.ScenarioDirect as PlainRecord
import qualified PlainRecord.ScenarioGetValueClass as PlainRecord
import qualified PlainRecord.ScenarioGetValueConcrete as PlainRecord
import qualified PlainRecord.Type as PlainRecord
import qualified PlainRecord.Value as PlainRecord

main :: IO ()
main =
  defaultMain
    [ bench "PlainRecord.sumDirect" $ nf' PlainRecord.sumDirect PlainRecord.oneRecord,
      bench "PlainRecord.sumGetValueConcrete" $ nf' PlainRecord.sumGetValueConcrete PlainRecord.oneRecord,
      bench "PlainRecord.sumGetValueClass" $ nf' PlainRecord.sumGetValueClass PlainRecord.oneRecord,
      bench "LinkedList.sumDirect" $ nf' LinkedList.sumDirect LinkedList.oneRecord,
      bench "LinkedList.sumGetValueConcrete" $ nf' LinkedList.sumGetValueConcrete LinkedList.oneRecord,
      bench "LinkedList.sumGetValueClass" $ nf' LinkedList.sumGetValueClass LinkedList.oneRecord
    ]

nf' :: (NFData b, Num b) => (a -> b) -> a -> Benchmarkable
nf' f = nf (sum . map f) . replicate 1000000
