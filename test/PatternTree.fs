module Test.PatternTree

open Expecto
open Swensen.Unquote

open PatternTree

[<Tests>]
let tests =
    testList "PatternTree" [
        testCase "Small test" <| fun () ->
            let target = set [
                ("t0", "class", "t0")
                ("t0", "contains", "t01")
                ("t01", "method", "t01")
                ("t0", "contains", "t02")
                ("t02", "method", "t02")
                ("t02", "public", "t02")
                ("t0", "contains", "t0a")
                ("t0a", "field", "t0a")
                ("t0a", "public", "t0a")
                ("t1", "class", "t1")
                ("t1", "contains", "t11")
                ("t11", "method", "t11")
                ("t1", "contains", "t1a")
                ("t1a", "field", "t1a")
                ("t1a", "private", "t1a")
                ("t2", "class", "t2")
                ("t2", "contains", "t21")
                ("t21", "public", "t21")
                ("t21", "method", "t21")
                ("t2", "contains", "t22")
                ("t22", "method", "t22")
                ("t22", "private", "t22")
            ]

            let patterns =
                { Verdict = Positive
                  Pattern = set [ 
                    (0, "class", 0) 
                  ]
                  Children = [
                    { Verdict = Negative
                      Pattern = set [
                        (1, "method", 1)
                        (1, "public", 1)
                        (0, "contains", 1)
                      ]
                      Children = [] }
                    { Verdict = Neutral
                      Pattern = set [
                        (1, "field", 1)
                        (0, "contains", 1)
                      ]
                      Children = [] }
                  ] }

            let results = set (PatternTree.searchSimple target patterns)

            let expected = set [ 
                Negative, Map.ofList [ (0, "t0"); (1, "t02") ]
                Negative, Map.ofList [ (0, "t2"); (1, "t21") ]
                Neutral, Map.ofList [ (0, "t0"); (1, "t0a") ]
                Neutral, Map.ofList [ (0, "t1"); (1, "t1a") ] 
            ]
            
            test <@ results = expected @>
    ]
