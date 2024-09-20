module Apollopp

open Argu
open DirectedSuMGra
open Graph
open Microsoft.ClearScript.V8
open MultiGraph
open PatternTree
open Polly
open QueryBuilder
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reactive.Linq
open System.Threading.Tasks
open Thoth.Json.Net
open TypeGraph
open Polly.Retry

module Task =
    let map f (t: Task<'t>) =
        task { let! res = t in return f res }

    let wait (t: Task<'a>) =
        t.GetAwaiter().GetResult()

type Criterion<'pattern> =
    { Criterion: string
      Patterns: PatternTree<'pattern> list }

module Criterion =
    let mapiPattern f criterion =
        { Criterion = criterion.Criterion
          Patterns = List.mapi f criterion.Patterns }

    let encode (encodePattern: Encoder<'pattern>) : Encoder<Criterion<'pattern>> =
        fun criterion -> Encode.object [
            "criterion", Encode.string criterion.Criterion
            "patterns", criterion.Patterns |> List.map (PatternTree.encode encodePattern) |> Encode.list
        ]

    let decode (decodePattern: Decoder<'pattern>) : Decoder<Criterion<'pattern>> =
        Decode.object <| fun get ->
            { Criterion = get.Required.Field "criterion" Decode.string
              Patterns = get.Required.Field "patterns" (Decode.list (PatternTree.decode decodePattern)) }

let makeTarget (id: string) typegraphFile =
    task {
        let! json = File.ReadAllTextAsync typegraphFile
        return 
            Decode.fromString (TypeGraph.decode Decode.string) json
            |> Result.map (Target.fromGraph id)
    }

let makeTargets (graphPath: string) (skip: int option) (limit: int option) (targetsDir: string) =
    task {
        let! results =
            Directory.EnumerateDirectories targetsDir
            |> match skip with Some n -> Seq.skip n | None -> id
            |> match limit with Some n -> Seq.truncate n | None -> id
            |> Seq.map (fun projectDir ->
                let typegraphFile = Path.Combine(projectDir, graphPath, "typegraph.json")
                let id = Path.GetFileName (Path.TrimEndingDirectorySeparator projectDir)
                makeTarget id typegraphFile |> Task.map (Result.mapError (fun err -> id, err))
            )
            |> Task.WhenAll
        return
            results
            |> Seq.map (Result.mapError (fun (id, err) -> printfn "Error loading target %s: %s" id err))
            |> Seq.choose Result.toOption
            |> Seq.toList
    }

let makeCriterion =
    let retry =
        ResiliencePipelineBuilder()
            .AddRetry(RetryStrategyOptions(MaxRetryAttempts = 10, Delay = TimeSpan.FromMilliseconds(100)))
            .Build()

    fun (file: string) -> task {
        let! content =
            retry.ExecuteAsync(fun ct -> File.ReadAllTextAsync(file, ct) |> ValueTask<string>)
        let json =
            match Path.GetExtension file with
            | ".json" ->
                content
            | ".js" ->
                use jsEngine = new V8ScriptEngine()
                jsEngine.Execute(content)
                jsEngine.Evaluate("JSON.stringify(criterion)") :?> string
            | _ ->
                failwith "Unknown file type."
        return
            Decode.fromString (Criterion.decode (TypeGraph.decode Decode.string)) json
            |> Result.map (Criterion.mapiPattern (fun i -> PatternTree.buildQueries $"query_%d{i}"))
    }

let makeCriteria (criteriaDir: string) =
    task {
        let! results =
            Directory.EnumerateFiles criteriaDir
            |> Seq.map (fun file ->
                makeCriterion file |> Task.map (Result.mapError (fun err -> file, err))
            )
            |> Task.WhenAll
        return
            results
            |> Seq.choose Result.toOption
            |> Seq.toList
    }

type RunResult = (Verdict * (Query<string, TypeGraphEdge> * Map<int, int>) array) array

let run (criterion: Criterion<Query<string, TypeGraphEdge>>) (target: Target<string, TypeGraphEdge>) : RunResult =
    Seq.collect (PatternTree.search target) criterion.Patterns
    |> Seq.groupBy (fun (verdict, _, _) -> verdict)
    |> Seq.map (fun (verdict, results) ->
        let results =
            results
            |> Seq.map (fun (_, query, mapping) -> query, mapping)
            |> Seq.toArray
        verdict, results
    )
    |> Seq.sortBy fst
    |> Seq.toArray

let runAllCriteria (criteria: #seq<Criterion<Query<string, TypeGraphEdge>>>) (target: Target<string, TypeGraphEdge>) =
    criteria
    |> Seq.map (fun criterion -> task { return criterion.Criterion, run criterion target })
    |> Task.WhenAll

let runAllTargets (targets: #seq<Target<string, TypeGraphEdge>>) (criterion: Criterion<Query<string, TypeGraphEdge>>) =
    targets
    |> Seq.map (fun target -> task { return target, run criterion target })
    |> Task.WhenAll

let runAll (targets: #seq<Target<string, TypeGraphEdge>>) (criteria: #seq<Criterion<Query<string, TypeGraphEdge>>>) =
    targets 
    |> Seq.map (fun target ->
        task {
            let! results = runAllCriteria criteria target
            return target, results 
        }
    ) 
    |> Task.WhenAll

let writeRunResult (target: Target<string, TypeGraphEdge>) (results: RunResult) (indent: int) (writer: TextWriter) =
    task {
        let indentation = String(' ', indent)
        do! writer.WriteAsync(indentation)
        do! writer.WriteLineAsync(
            results
            |> Seq.map (fun (verdict, mappings) -> $"{verdict}: {mappings.Length}") 
            |> String.concat " / "
        )

        for verdict, results in results do
            if verdict <> Neutral then
                for query, mapping in results do
                    do! writer.WriteLineAsync($"{indentation}- {verdict} / {query.Id}")
                    for (KeyValue (qnode, tnode)) in mapping do
                        do! writer.WriteLineAsync($"{indentation}  - `{query.NodeArray[qnode]}` ->")
                        do! writer.WriteLineAsync($"{indentation}    `{target.NodeArray[tnode]}`")
    }

let writeTargetResults (target: Target<string, TypeGraphEdge>) (results: (string * RunResult) array) (indent: int) (writer: TextWriter) =
    task {
        let indentation = String(' ', indent)
        for criterion, results in results do
            do! writer.WriteAsync($"{indentation}- ")
            do! writer.WriteLineAsync(criterion)
            do! writeRunResult target results (indent + 2) writer
    }

let writeCriterionResults (results: (Target<string,TypeGraphEdge> * RunResult) array) (indent: int) (writer: TextWriter) =
    task {
        let indentation = String(' ', indent)
        for target, result in results do
            do! writer.WriteAsync($"{indentation}- ")
            do! writer.WriteLineAsync(target.Id)
            do! writeRunResult target result (indent + 2) writer

        let stats = 
            let targetCount = results |> Array.length
            results
            |> Array.collect snd
            |> Array.groupBy fst
            |> Array.map (fun (verdict, results) ->
                verdict, float (results |> Array.collect snd |> Array.length) / float targetCount
            )
            |> Array.sortBy fst

        do! writer.WriteLineAsync($"{indentation}- Average:")
        for verdict, avg in stats do
            do! writer.WriteLineAsync($"{indentation}  %A{verdict}: %.1f{avg}")
    }

type UniqueAsyncQueue<'t>() =
    let itemQueue = Queue<'t>()
    let requestQueue = Queue<TaskCompletionSource<'t>>()

    member this.Enqueue (item: 't) =
        lock this <| fun () ->
            if requestQueue.Count > 0 then
                requestQueue.Dequeue().SetResult item
            elif not (itemQueue.Contains item) then
                itemQueue.Enqueue item

    member this.DequeueAsync () =
        lock this <| fun () ->
            if itemQueue.Count > 0 then
                itemQueue.Dequeue()
                |> Task.FromResult
            else
                let tcs = TaskCompletionSource<'t>()
                requestQueue.Enqueue tcs
                tcs.Task

let watchCriteria (time: bool) (run: Criterion<Query<string, TypeGraphEdge>> -> Task<unit>) (criteriaDir: string) =
    task {
        use watcher = new FileSystemWatcher(criteriaDir)
        watcher.IncludeSubdirectories <- true

        let events = 
            Observable.FromEventPattern<FileSystemEventHandler, FileSystemEventArgs>(
                (fun handler -> watcher.Changed.AddHandler handler), 
                (fun handler -> watcher.Changed.RemoveHandler handler)
            )

        let fileQueue = UniqueAsyncQueue<string>()
        use subscription =
            events
                .GroupBy(fun event -> event.EventArgs.FullPath)
                .SelectMany(fun group -> group.Throttle(TimeSpan.FromMilliseconds(100)))
                .Subscribe(fun event -> fileQueue.Enqueue event.EventArgs.FullPath)

        watcher.EnableRaisingEvents <- true

        let stopwatch = Stopwatch()

        while true do
            let! file = fileQueue.DequeueAsync()
            printfn ""
            printfn "%s> %s" (DateTime.Now.ToShortTimeString()) file
            try
                stopwatch.Restart()
                match! makeCriterion file with
                | Ok criterion ->
                    do! run criterion
                | Error err ->
                    printfn "Error loading criterion: %s" err
                stopwatch.Stop()
                if time then printfn "-> Done in %dms" stopwatch.ElapsedMilliseconds
            with err ->
                stopwatch.Stop()
                printfn "Error running criterion: %s" err.Message
    }

let findExtensions (results: (Target<string,TypeGraphEdge> * RunResult) array) =
    let inline selectExtensions (extensions: 't seq when 't : (member Fraction : float)) =
        seq {
            let mutable highCount = 0
            let mutable middleCount = 0
            for ext in extensions do
                if highCount < 3 then
                    highCount <- highCount + 1
                    yield ext
                elif middleCount < 3 && ext.Fraction < 0.6 then
                    middleCount <- middleCount + 1
                    yield ext
        }

    results
    |> Seq.collect (fun (target, runResults) ->
        runResults 
        |> Seq.collect snd
        |> Seq.map (fun (query, mapping) -> query, target, mapping)
    )
    |> Seq.groupBy (fun (query, _, _) -> query)
    |> Seq.map (fun (query, mappings) -> 
        let mappings = Seq.map (fun (_, target, mapping) -> target, mapping) mappings |> Seq.toArray
        let edgeExtensions = 
            QueryBuilder.findEdgeExtensions (MultiGraph.toIntGraph query.Graph) mappings
            |> selectExtensions
        let nodeExtensions =
            QueryBuilder.findNodeExtensions query.Graph mappings
            |> selectExtensions
        query, edgeExtensions, nodeExtensions)
    |> Seq.sortBy (fun (query, _, _) -> query.Id)

let writeExtensions (extensions: #seq<Query<string, TypeGraphEdge> * #seq<EdgeExtension<TypeGraphEdge>> * #seq<NodeExtension<string, TypeGraphEdge>>>) (indent: int) (writer: TextWriter) =
    task {
        let indentation = String(' ', indent)

        do! writer.WriteLineAsync($"{indentation}Suggested extensions:")
        for query, edgeExtensions, nodeExtensions in extensions do
            do! writer.WriteLineAsync($"{indentation}- %s{query.Id}")
            do! writer.WriteLineAsync($"{indentation}  Edges:")
            for ext in edgeExtensions do
                do! writer.WriteLineAsync($"{indentation}  - [%.2f{ext.Fraction}]")
                do! writer.WriteLineAsync($"{indentation}    %A{query.NodeArray[Edge.from ext.Edge]}")
                do! writer.WriteLineAsync($"{indentation}    > %A{Edge.edge ext.Edge} >")
                do! writer.WriteLineAsync($"{indentation}    %A{query.NodeArray[Edge.to' ext.Edge]}")
            do! writer.WriteLineAsync($"{indentation}  Nodes:")
            for ext in nodeExtensions do
                do! writer.WriteLineAsync($"{indentation}  - [%.2f{ext.Fraction}]")
                do! writer.WriteLineAsync($"{indentation}    %A{query.NodeArray[ext.QueryNode]}")
                for edge in ext.Outgoing do
                    do! writer.WriteLineAsync($"{indentation}    > %A{edge} >")
                for edge in ext.Incoming do
                    do! writer.WriteLineAsync($"{indentation}    < %A{edge} <")
                for edge in ext.AdjacentLoops do
                    do! writer.WriteLineAsync($"{indentation}    | %A{edge} |")
                for target, _, targetNode in ext.Occurrences |> Seq.distinctBy (fun (target, _, _) -> target.Id) |> Seq.truncate 3 do
                    do! writer.WriteLineAsync($"{indentation}    = %A{target.NodeArray[targetNode]}")
                    do! writer.WriteLineAsync($"{indentation}      in %A{target.Id}")
    }

type ConfigureArgs =
    | [<Unique; AltCommandLine("-o")>] Output of path: string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Output _ -> "Output file for results"

type RunArgs =
    | [<Unique; AltCommandLine("-o")>] Output of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Output _ -> "Output directory for the results"

type Args =
    | [<CliPrefix(CliPrefix.None)>] Configure of ParseResults<ConfigureArgs>
    | [<CliPrefix(CliPrefix.None)>] Run of ParseResults<RunArgs>
    | [<Inherit; Mandatory; Unique; AltCommandLine("-c")>] Criteria of path: string
    | [<Inherit; Mandatory; Unique; AltCommandLine("-t")>] Targets of path: string
    | [<Inherit; Unique>] Graph_Path of path: string
    | [<Inherit; Unique>] Skip of n: int
    | [<Inherit; Unique>] Limit of n: int
    | [<Inherit; Unique>] Time

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Configure _ -> "Interactively configure patterns"
            | Run _ -> "Run patterns on targets"
            | Criteria _ -> "Path to the directory containing JSON criterion files"
            | Targets _ -> "Path to the directory containing target projects"
            | Graph_Path _ -> "Path to the directory within a project containing JSON graph files, defaults to source/graph"
            | Skip _ -> "Skip the first n targets"
            | Limit _ -> "Limit the number of targets to n"
            | Time -> "Time the duration of operations and report the results"

[<EntryPoint>]
let main args =
    let argParser = ArgumentParser.Create<Args>()
    try
        let args = argParser.ParseCommandLine(inputs = args, raiseOnUsage = true)

        let graphPath = 
            args.TryGetResult <@ Graph_Path @>
            |> Option.defaultValue "source/graph"
        let skip = args.TryGetResult <@ Skip @>
        let limit = args.TryGetResult <@ Limit @>

        let targetsDir = args.GetResult <@ Targets @>
        let criteriaDir = args.GetResult <@ Criteria @>

        match args.TryGetSubCommand() with
        | Some (Configure confArgs) ->
            Task.wait <| task {
                printf "Reading targets... "
                let! targets = makeTargets graphPath skip limit targetsDir
                printfn "Done."

                let getOutWriter =
                    match confArgs.TryGetResult <@ ConfigureArgs.Output @> with
                    | Some outputFile ->
                        fun _ ->
                            let file = File.Open(outputFile, FileMode.Create, FileAccess.Write, FileShare.Read)
                            let writer = new StreamWriter(file)
                            let dispose = fun _ -> writer.Dispose(); file.Dispose()
                            writer :> TextWriter, dispose
                    | None ->
                        fun _ -> Console.Out, fun _ -> ()
                
                let run criterion =
                    task {
                        let outWriter, disposeOutWriter = getOutWriter ()
                        let! results = runAllTargets targets criterion
                        do! writeCriterionResults results 0 outWriter
                        let extensions = findExtensions results
                        do! writeExtensions extensions 0 outWriter
                        disposeOutWriter ()
                    }
                
                printfn "Start watching %s." criteriaDir
                do! watchCriteria (args.Contains <@ Time @>) run criteriaDir
            }
            0
        | Some (Run runArgs) ->
            Task.wait <| task {
                let stopwatch = Stopwatch()

                let printDone () =
                    stopwatch.Stop()
                    if args.Contains <@ Time @>
                    then printfn "Done. Took %dms" stopwatch.ElapsedMilliseconds
                    else printfn "Done."

                printf "Reading targets... "
                stopwatch.Start()
                let! targets = makeTargets graphPath skip limit targetsDir
                printDone ()

                printf "Reading criteria... "
                stopwatch.Restart()
                let! criteria = makeCriteria criteriaDir
                printDone ()

                let getWriter: string -> TextWriter * (unit -> unit) =
                    match runArgs.TryGetResult <@ RunArgs.Output @> with
                    | Some outputDir ->
                        if not (Directory.Exists outputDir) then Directory.CreateDirectory outputDir |> ignore
                        fun targetId ->
                            let path = Path.Combine(outputDir, $"{Path.GetFileName (Path.TrimEndingDirectorySeparator targetId)}.md")
                            let file = File.CreateText(path)
                            file, file.Dispose
                    | None ->
                        fun targetId ->
                            printfn ""
                            printfn "# %s" targetId
                            Console.Out, fun () -> ()
                
                if args.Contains <@ Time @>
                then printf "Assessing %d projects on %d criteria... " targets.Length criteria.Length
                else printf "Asesssing projects... "
                stopwatch.Restart()
                let! results = runAll targets criteria
                for targets, results in results do
                    let writer, dispose = getWriter targets.Id
                    try
                        do! writeTargetResults targets results 0 writer
                    finally
                        dispose ()
                printDone ()
            }
            0
        | Some _ | None ->
            printfn "%s" (argParser.PrintUsage(message = "Please specify a subcommand"))
            1
    with e ->
        printfn "Exception: %s" e.Message
        printfn "%s" e.StackTrace
        1
