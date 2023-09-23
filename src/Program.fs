module Apollopp

open DirectedSuMGra
open Graph
open MultiGraph
open QueryBuilder
open System.IO
open Thoth.Json.Net
open TypeGraph
open PatternTree

let prepareTargets (projectsDir: string) =
    let times = ResizeArray()
    let stpwtch = System.Diagnostics.Stopwatch()
    for project in Directory.EnumerateDirectories(projectsDir) do
        Path.Combine(project, "source", "graph", "typegraph.json")
        |> File.ReadAllText
        |> Decode.fromString (TypeGraph.decoder Decode.string)
        |> Result.iter (fun graph ->
            File.Delete(Path.Combine(project, "source", "graph", "typegraph_nodes.json"))
            printfn "Preparing %s" project
            stpwtch.Start()
            let target = Target.fromGraph project graph
            stpwtch.Stop()
            times.Add(stpwtch.Elapsed.TotalSeconds)
            printfn "  %.3fs" stpwtch.Elapsed.TotalSeconds
            stpwtch.Reset()
            let targetPath = Path.Combine(project, "source", "graph", "typegraph_target.json")
            let targetJson = target |> Target.encode Encode.string TypeGraphEdge.encoder |> Encode.toString 0
            File.WriteAllText(targetPath, targetJson)
        )
    times

let readTargets (projectsDir: string) =
    [ for project in Directory.EnumerateDirectories(projectsDir) ->
        Path.Combine(project, "source", "graph", "typegraph_target.json")
        |> File.ReadAllText
        |> Decode.fromString (Target.decode Decode.string TypeGraphEdge.decoder)
        |> function 
           | Ok target -> target 
           | Error err -> failwith $"Failed to decode typegraph_target for %s{project}: %s{err}" ]

let prepareTimes1 = prepareTargets "D:/Arthur/OneDrive/UTwente/Master/Y2Thesis/Data/AiC/submissions_cleaned_2020/assessed"
let prepareTimes2 = prepareTargets "D:/Arthur/OneDrive/UTwente/Master/Y2Thesis/Data/AiC/submissions_cleaned_2022/assessed"

// let readStpwtch = System.Diagnostics.Stopwatch()
// readStpwtch.Start()
// let targets2020 = readTargets "D:/Arthur/OneDrive/UTwente/Master/Y2Thesis/Data/AiC/submissions_cleaned_2020/assessed"
// let targets2022 = readTargets "D:/Arthur/OneDrive/UTwente/Master/Y2Thesis/Data/AiC/submissions_cleaned_2022/assessed"
// readStpwtch.Stop()

// printfn "Prepare time: %.3fs" prepareStpwtch.Elapsed.TotalSeconds
// printfn "Read time: %.3fs" readStpwtch.Elapsed.TotalSeconds

// let [ targets2020Config; targets2020Control ] = targets2020 |> List.splitInto 2

let patterns = [
    "Includes meaningful randomness (normal distribution and Perlin noise).", [
    { Verdict = Neutral
      Pattern = set [
        ("method", Annotated (InProjectDecl "java+method"), "method")
        ("method", Invokes, "randomMethod")
      ]
      Children = [
        { Verdict = Positive
          Pattern = set [ ("randomMethod", Annotated (ExternalDecl "java+method:///processing/core/PApplet/random(float,float)"), "randomMethod") ]
          Children = [] }
        { Verdict = Positive
          Pattern = set [ ("randomMethod", Annotated (ExternalDecl "java+method:///processing/core/PApplet/random(float)"), "randomMethod") ]
          Children = [] }
        { Verdict = Positive
          Pattern = set [ ("randomMethod", Annotated (ExternalDecl "java+method:///processing/core/PApplet/randomGaussian()"), "randomMethod") ]
          Children = [] }
        { Verdict = Positive
          Pattern = set [ ("randomMethod", Annotated (ExternalDecl "java+method:///processing/core/PApplet/noise(float)"), "randomMethod") ]
          Children = [] }
        { Verdict = Positive
          Pattern = set [ ("randomMethod", Annotated (ExternalDecl "java+method:///processing/core/PApplet/noise(float,float)"), "randomMethod") ]
          Children = [] }
        { Verdict = Positive
          Pattern = set [ ("randomMethod", Annotated (ExternalDecl "java+method:///processing/core/PApplet/noise(float,float,float)"), "randomMethod") ]
          Children = [] }
      ] }
    ]

    "Includes particles.", [
    { Verdict = Positive
      Pattern = set [
        ("Particle", Annotated (InProjectDecl "java+class"), "Particle")
        ("Particle", Contains, "lifespan")
        ("lifespan", Annotated (InProjectDecl "java+field"), "lifespan")
        ("Particle", Contains, "update")
        ("update", Annotated (InProjectDecl "java+method"), "update")
        ("update", AccessesField, "lifespan")
        ("Particle", Contains, "isDead")
        ("isDead", Annotated (InProjectDecl "java+method"), "isDead")
        ("isDead", AccessesField, "lifespan")
        ("isDead", DependsOn, "boolean")
        ("boolean", Annotated (ExternalDecl "java+primitiveType:///boolean"), "boolean")
        
        ("ParticleSystem", Annotated (InProjectDecl "java+class"), "ParticleSystem")
        ("ParticleSystem", Contains, "particles")
        ("particles", Annotated (InProjectDecl "java+field"), "particles")
        ("particles", DependsOn, "Particle")
        ("particles", DependsOn, "ArrayList")
        ("ArrayList", Annotated (ExternalDecl "java+class:///java/util/ArrayList"), "ArrayList")
        ("ParticleSystem", Contains, "run")
        ("run", AccessesField, "particles")
        ("run", Invokes, "isDead")
      ]
      Children = [
        { Verdict = Neutral
          Pattern = set [
            ("isDead", Contains, "someParam")
            ("someParam", Annotated (InProjectDecl "java+parameter"), "someParam")
          ]
          Children = [] }
      ] }
    ]

    "Includes flocking", [
    { Verdict = Positive
      Pattern = set [
        ("Flock", Annotated (InProjectDecl "java+class"), "Flock")
        ("Flock", DependsOn, "Boid")
        ("Flock", Contains, "run")
        ("run", Annotated (InProjectDecl "java+method"), "run")
        ("run", Invokes, "flockingMethod")
        ("Boid", Annotated (InProjectDecl "java+class"), "Boid")
        ("Boid", Contains, "flockingMethod")
        ("flockingMethod", Annotated (InProjectDecl "java+method"), "flockingMethod")
        ("flockingMethod", Contains, "boidsParam")
        ("boidsParam", Annotated (InProjectDecl "java+parameter"), "boidsParam")
        ("boidsParam", DependsOn, "ArrayList")
        ("ArrayList", Annotated (ExternalDecl "java+class:///java/util/ArrayList"), "ArrayList")
        ("boidsParam", DependsOn, "Boid")
      ]
      Children = [] }
    ]

    "Do not use unnecessary global variables, especially not for value passing.", [
    { Verdict = Negative
      Pattern = set [
        ("PApplet", Annotated (ExternalDecl "java+class:///processing/core/PApplet"), "PApplet")
        ("mainTab", Annotated (InProjectDecl "java+class"), "mainTab")
        ("mainTab", Extends, "PApplet")
        ("globalVar", Annotated (InProjectDecl "java+field"), "globalVar")
        ("mainTab", Contains, "globalVar")
        ("otherClass", Annotated (InProjectDecl "java+class"), "otherClass")
        ("otherClass", AccessesField, "globalVar")
      ]
      Children = [
        { Verdict = Neutral
          Pattern = set [
            ("globalVar", Annotated (Modifier "final"), "globalVar")
          ]
          Children = [] }
        { Verdict = Negative
          Pattern = set [
            ("otherClass2", Annotated (InProjectDecl "java+class"), "otherClass2")
            ("otherClass2", AccessesField, "globalVar")
          ]
          Children = [
            { Verdict = Neutral
              Pattern = set [
                ("globalVar", Annotated (Modifier "final"), "globalVar")
              ]
              Children = [] }
          ] }
      ] }
    ]

    "Do not hide user interaction in classes.", [
    { Verdict = Positive
      Pattern = set [
        ("PApplet", Annotated (ExternalDecl "java+class:///processing/core/PApplet"), "PApplet")
        ("mainTab", Annotated (InProjectDecl "java+class"), "mainTab")
        ("mainTab", Extends, "PApplet")
        ("mainTab", Contains, "method")
        ("method", Annotated (InProjectDecl "java+method"), "method")
        ("method", Overrides, "PApplet.method")
      ]
      Children = [
        { Verdict = Neutral
          Pattern = set [ ("PApplet.method", Annotated (ExternalDecl "java+method:///processing/core/PApplet/setup()"), "PApplet.method") ]
          Children = [] }
        { Verdict = Neutral
          Pattern = set [ ("PApplet.method", Annotated (ExternalDecl "java+method:///processing/core/PApplet/draw()"), "PApplet.method") ]
          Children = [] }
        { Verdict = Neutral
          Pattern = set [ ("PApplet.method", Annotated (ExternalDecl "java+method:///processing/core/PApplet/settings()"), "PApplet.method") ]
          Children = [] }
      ] }
    { Verdict = Neutral
      Pattern = set [
        ("method", AccessesField, "field")
        ("method", Annotated (InProjectDecl "java+method"), "method")
      ]
      Children = 
        [ "key"; "keyCode"; "keyPressed"; "mouseButton"; "mousePressed"; "mouseX"; "mouseY"; "pmouseX"; "pmouseY" ]
        |> List.map (fun field -> 
            { Verdict = Negative
              Pattern = set [ ("field", Annotated (ExternalDecl $"java+field:///processing/core/PApplet/%s{field}"), "field") ]
              Children = 
                [ "keyPressed()"; "keyReleased()"; "keyTyped()"; "mouseClicked()"; "mouseDragged()"; "mouseMoved()"; "mousePressed()"; "mouseReleased()"; "mouseWheel()" ]
                |> List.map (fun method ->
                    { Verdict = Neutral
                      Pattern = set [
                        ("PApplet", Annotated (ExternalDecl "java+class:///processing/core/PApplet"), "PApplet")
                        ("mainTab", Annotated (InProjectDecl "java+class"), "mainTab")
                        ("mainTab", Extends, "PApplet")
                        ("mainTab", Contains, "method")
                        ("method", Overrides, "PApplet.method")
                        ("PApplet.method", Annotated (ExternalDecl $"java+method:///processing/core/PApplet/%s{method}"), "PApplet.method")
                      ]
                      Children = [] }
                ) }
        ) }
    ]

    "How many classes are there?", [
    { Verdict = Positive
      Pattern = set [
        ("class", Annotated (InProjectDecl "java+class"), "class")
      ]
      Children = [] }
    ]

    "How much interaction is between the classes. Does an event in one class have an effect on objects of other class?", [
    { Verdict = Positive
      Pattern = set [
        ("class1", Annotated (InProjectDecl "java+class"), "class1")
        ("class2", Annotated (InProjectDecl "java+class"), "class2")
        ("class2", Contains, "method2")
        ("method2", Annotated (InProjectDecl "java+method"), "method2")
        ("class1", Invokes, "method2")
      ]
      Children = [
        { Verdict = Neutral
          Pattern = set [ 
            ("PApplet", Annotated (ExternalDecl "java+class:///processing/core/PApplet"), "PApplet")
            ("class1", Extends, "PApplet")
          ]
          Children = [] } 
      ] }
    ]

    "Do not have unused code", [
    { Verdict = Negative
      Pattern = set [
        ("method", Annotated (InProjectDecl "java+method"), "method")
      ]
      Children = [
        { Verdict = Neutral
          Pattern = set [
            ("other", Invokes, "method")
          ]
          Children = [] }
        { Verdict = Neutral
          Pattern = set [
            ("method", Overrides, "other")
          ]
          Children = [] }
        { Verdict = Neutral
          Pattern = set [
            ("PApplet.main", Annotated (ExternalDecl "java+method:///processing/core/PApplet/main(java.lang.String%5B%5D)"), "PApplet.main")
            ("method", Invokes, "PApplet.main")
          ]
          Children = [] }
      ] }
    ]

    "Use functions when you have similar code.", [
    { Verdict = Positive
      Pattern = set [
        ("reusedMethod", Annotated (InProjectDecl "java+method"), "reusedMethod")
        ("caller1", Invokes, "reusedMethod")
        ("caller1", Annotated (InProjectDecl "java+method"), "caller1")
        ("caller2", Invokes, "reusedMethod")
        ("caller2", Annotated (InProjectDecl "java+method"), "caller2")
      ]
      Children = [] }
    ]
]

// let patternQueries = 
//     patterns
//     |> List.map (fun (criterion, patterns) ->
//         criterion, List.map PatternTree.buildQueries patterns
//     )

// let projects = List.append targets2020Control targets2022
// let times = ResizeArray()

// for projectTarget in projects do
//     printfn "%s" projectTarget.Id
//     let stpwtch = System.Diagnostics.Stopwatch()
//     stpwtch.Start()
//     use resultsFile = File.CreateText(Path.Combine(projectTarget.Id, "apollopp.md"))
//     for criterion, patterns in patternQueries do
//         resultsFile.Write("## "); resultsFile.WriteLine(criterion)
//         let results =
//             Seq.collect (PatternTree.search projectTarget) patterns
//             |> Seq.groupBy (fun (verdict, _, _) -> verdict)
//             |> Seq.map (fun (verdict, results) ->
//                 let results =
//                     results
//                     |> Seq.map (fun (_, query, mapping) -> query, mapping)
//                     |> Seq.toArray
//                 verdict, results
//             )
//             |> Seq.toArray
//         results 
//         |> Seq.map (fun (verdict, mappings) -> $"{verdict}: {mappings.Length}") 
//         |> String.concat " / "
//         |> resultsFile.WriteLine
//         for verdict, results in results do
//             if verdict <> Neutral then
//                 for query, mapping in results do
//                     resultsFile.WriteLine($"- {verdict} / {query.Id}")
//                     for (KeyValue (qnode, tnode)) in mapping do
//                         resultsFile.WriteLine($"  - `{query.NodeArray[qnode]}` ->")
//                         resultsFile.WriteLine($"    `{projectTarget.NodeArray[tnode]}`")
//     stpwtch.Stop()
//     printfn "%.3fs" stpwtch.Elapsed.TotalSeconds
//     times.Add(stpwtch.Elapsed.TotalSeconds)

let printStatistics times =
    printfn "Timings:"
    printfn " Min: %.3fs" (times |> Seq.min)
    printfn " Max: %.3fs" (times |> Seq.max)
    let average = times |> Seq.average
    printfn " Average: %.3fs" average
    let stddev = 
        let diffSqSum = Seq.sumBy (fun time -> pown (time - average) 2) times
        sqrt (diffSqSum / float (Seq.length times - 1))
    printfn " StdDev: %.3fs" stddev

// printStatistics times

let prepareTimes = Seq.append prepareTimes1 prepareTimes2
printStatistics prepareTimes
prepareTimes |> Seq.sort |> Seq.iter (printfn "%.3fs")

// let results = 
//     targets2020Config
//     |> List.map (fun target -> 
//         let results =
//             PatternTree.search target patternQuery 
//             |> Seq.groupBy (fun (verdict, _, _) -> verdict)
//             |> Seq.map (fun (verdict, results) -> 
//                 let results =
//                     results 
//                     |> Seq.map (fun (_, query, mapping) -> query, mapping)
//                     |> Seq.toList
//                 verdict, results
//             )
//             |> Seq.toList
//         target, results
//     )

// for target, results in results do
//     printfn "- %s" target.Id
//     for verdict, results in results do
//         printfn "  %A: %i" verdict (results |> List.length)
//         for query, mapping in results do
//             for i, (qnode, tnode) in mapping |> Map.toSeq |> Seq.indexed do
//                 if i = 0 then printf "  - " else printf "    "
//                 printfn "(%s) %A -> %s" query.Id query.NodeArray[qnode] target.NodeArray[tnode]

// let stats = 
//     let targetCount = results |> List.length
//     results
//     |> List.collect snd
//     |> List.groupBy fst
//     |> List.map (fun (verdict, results) ->
//         verdict, float (results |> List.collect snd |> List.length) / float targetCount
//     )

// printfn "- Average:"
// for verdict, avg in stats do
//     printfn "  %A: %.1f" verdict avg

// for mapping in mappings do
//     printfn "Mapping:"
//     for key, value in mapping |> Map.toSeq |> Seq.sortBy fst do
//         printfn "%2i -> %2i (%i -> %s)" key value queryNodes.[key] targetNodes.[value]

// printfn "Suggested edges:"
// for ext in QueryBuilder.findEdgeExtensions queryGraph (mappings |> Seq.map (fun mapping -> target, mapping)) do
//     printfn "%A" ext

// printfn "Suggested nodes:"
// for ext in QueryBuilder.findNodeExtensions queryGraph (mappings |> Seq.map (fun mapping -> target, mapping)) do
//     printfn "- %f: %A -> %A\n  Outgoing: %A\n  Incoming: %A" ext.Fraction ext.QueryNode ext.AdjacentLoops ext.Outgoing ext.Incoming
//     printfn "  Occurrences:"
//     for target, mapping, node in ext.Occurrences do
//         printfn "  - %A: %A -> (new) %A" (target.Graph.Length) (mapping.[ext.QueryNode]) node
