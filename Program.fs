open FSharpPlus
open System.IO
open System
open AsyncResult

let readText path =  
    File.ReadAllTextAsync path 
    |> Async.AwaitTask

async {
    let! path = readText "test/path.txt"
    let! text = readText path
    printfn "Imperative: %s" text
} |> Async.RunSynchronously

let readFromPathfile = 
    readText "test/path.txt"
    >>= readText

async {
    let! text = readFromPathfile
    printfn "With bind op: %s" text
} |> Async.RunSynchronously


let readFromRead = readText >=> readText

async {
    let! text = readFromRead "test/path.txt"
    printfn "With Kleisli op: %s" text
} |> Async.RunSynchronously

let readTextResult path : AsyncResult<string, Exception> =  
    async {
        let! res = File.ReadAllTextAsync path  |> Async.AwaitTask |> Async.Catch
        return 
            match res with
            | Choice1Of2 r -> Ok r
            | Choice2Of2 e -> Error e
    } |> toAsyncResultAwait
    
async {
    match! 
        "test/path.txt" 
        |> readTextResult 
        >>= readTextResult   
        |> AsyncResult<_,_>.toResult 
    with
    | Ok ok -> printfn "With AsyncResult: %s" ok
    | Error e -> printfn "Error"
} |> Async.RunSynchronously

let readFromReadResult = readTextResult >=> readTextResult

async {
    match! 
        readFromReadResult "test/path.txt"
        |> AsyncResult<_,_>.toResult 
    with
    | Ok ok -> printfn "With AsyncResult Kleisli: %s" ok
    | Error e -> printfn "Error: %O" e
} |> Async.RunSynchronously

async {
    match! 
        readFromReadResult "test/not existing.txt"
        |> AsyncResult<_,_>.toResult 
    with
    | Ok ok -> printfn "With AsyncResult Kleisli: %s" ok
    | Error e -> printfn "With AsyncResult Kleisli, Error: %O" e
} |> Async.RunSynchronously

let tryParseInt : string -> int option = tryParse
let tryDivide x n = 
    if n = 0 then 
        None 
    else 
        Some (x / n)

let fisch = tryParseInt >=> tryDivide 456000

match fisch "100" with
| Some s ->  printfn "%d" s
| None -> printfn "nix"

let tp = 
    tryParse<int> "1000" 
    >>= tryDivide 345000






