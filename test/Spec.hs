import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Common.Math

-- TODO: write property tests

testWikiHamming1 = assertEqual
    "Hamming distance between 'karolin' and 'kathrin' is 3."
    3 (hammingDist "karolin" "kathrin")

testWikiHamming2 = assertEqual
    "Hamming distance between 'karolin' and 'kerstin' is 3"
    3 (hammingDist "karolin" "kerstin")

testWikiHamming3 = assertEqual
    "Hamming distance between '1011101' and '1001001' is 2"
    2 (hammingDist "1011101" "1001001")

testWikiHamming4 = assertEqual
    "Hamming distance between '2173896' and '2233796' is 3."
    3 (hammingDist "2173896" "2233796")

main = defaultMain tests

tests = [ testGroup "Hamming 1" [
              testCase "wiki_1" testWikiHamming1
            , testCase "wiki_2" testWikiHamming2
            , testCase "wiki_3" testWikiHamming3
            , testCase "wiki_4" testWikiHamming4
            ]
        ]
