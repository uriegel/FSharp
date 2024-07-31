open FSharpPlus
open System.IO
open System
open AsyncResult
open FSharpPlus.Data.Validation

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


let readTextResult path : Async<Result<string, Exception>> =  
    try 
        File.ReadAllTextAsync path 
        |> Async.AwaitTask
        |> map (fun x -> Ok x)
    with 
        | e -> async { return Error e }

let ar = readTextResult "test/path.txt" |> toAsyncResultAwait
let rtra path = readTextResult path |> toAsyncResultAwait
let readTextAwait = ar >>= rtra
async {
    let! text = readTextAwait |> AsyncResult<_,_>.toResult
    match text with
    | Ok ok -> printfn "With AsyncResult: %s" ok
    | Error e -> printfn "Error"
} |> Async.RunSynchronously


type Birds = int
type Pole = (Birds * Birds)

// landLeft :: Birds -> Pole -> Pole
let landLeft (n) ((left, right): Pole) = (left + n, right)
// landRight :: Birds -> Pole -> Pole
let landRight n ((left, right) : Pole) = (left, right + n)


let res55 =                                                               // [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')]
    monad {
        let! x = [1;2]
        let! y = [(x, 'a'); (x, 'b')]
        return y
    }


let landLeft1 n ((left, right) : Pole) =
    match abs ( (left + n) - right) with
    | x when x < 4 -> Some (left + n, right)
    | _ -> None

let landRight1 n ((left, right) : Pole) =
    match abs ( left - (right + n) ) with
    | x when x < 4 -> Some (left, right + n)
    | _ -> None


type Pair<'a, 'b> = Pair of ('a * 'b)
type Pair<'a, 'b> with
    static member runPair (Pair tuple : Pair<'a, 'b>) : ('a * 'b) = tuple
    static member map (f: 'a -> 'b) (Pair (a, b) : Pair<'a, 'a>) : Pair<'b, 'b> = Pair ((f a), (f b))
    static member bind f (Pair (a, b) : Pair<'a, 'a>) : Pair<'b, 'b> = 
        Pair ((f a), (f b))
    static member (>>=) ((Pair (a, b) : Pair<'a, 'a>), f) = Pair ((f a), (f b))
    static member Plus (a: int) (b: int) = a + b
    

    static member Map (x:Pair<'a,'a>, f) = Pair.map f x
    

//let (>>=) (f) ((Pair (a, b) : Pair<'a, 'a>)) = Pair ((f a), (f b))
let res19 = Pair (10, "hello")                                        // Pair (10, "hello")
//let res20 = (Pair (2, 3)) >>= ((*) 100)


type Affe<'a> = {
    length: int
    awert: 'a
}
type Affe<'a> with
    static member bind (f: 'a -> Affe<'b>) (affe: Affe<'a>) : Affe<'b> = 
        f affe.awert
    static member (>>=) ((affe: Affe<'a>), (f: 'a -> Affe<'b>)) = 
        f affe.awert 
    //static member (>>=) ((Pair (a, b) : Pair<'a, 'a>), f) = Pair ((f a), (f b))
    

let gibAffe (a: int) = { length = 0; awert =  sprintf "Schön: %d" (a + 77) }
let affe6 = { length = 90; awert =  6 }

let res20 = affe6 >>= gibAffe

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let affe = "4"
let schein = "Hund"
let a = affe + schein
printfn "%s" a

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






