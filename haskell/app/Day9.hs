module Day9 where
import Util (splitAtAll)

type Sequence = [Int]
type History = Sequence

test =
  ["0 3 6 9 12 15",
   "1 3 6 10 15 21",
  "10 13 16 21 30 45"
  ]

loadHistory :: String -> History
loadHistory = fmap read . splitAtAll ' '

reduceSequence :: Sequence -> Sequence
reduceSequence [_] = []
reduceSequence (h1 : h2 : hs) = h2 - h1 : reduceSequence (h2 : hs)

gatherSequences :: History -> [Sequence]
gatherSequences h = h :
  if any (/=0) reduced
    then
      gatherSequences reduced
    else
      []
  where
    reduced = reduceSequence h

makePrediction :: [Sequence] -> Int
makePrediction = foldl (\a b -> a + last b) 0

makePrediction2 :: [Sequence] -> Int
makePrediction2 = foldr (\a b -> head a - b) 0

day9_1 = show . sum . fmap (makePrediction . gatherSequences . loadHistory) . lines
day9_2 = show . sum . fmap (makePrediction2 . gatherSequences . loadHistory) . lines
