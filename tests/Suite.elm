module Suite exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Cell" cellTests


cellTests : List Test
cellTests =
    [ test "noop" <|
        \_ -> Expect.equal True True
    ]
