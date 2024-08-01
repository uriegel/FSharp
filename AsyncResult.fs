module AsyncResult
open System
open System.Threading.Tasks

type AsyncResult<'a, 'e> = {
    value: Async<Result<'a, 'e>>
}

let toAsyncResultAwait (ar: Async<Result<'a, 'e>>): AsyncResult<'a, 'e> = {value = ar }
let toAsyncResult (r: Result<'a, 'e>): AsyncResult<'a, 'e> = {value = async { return r } }
let toResult a = a.value

let catch (func: unit -> Task<'a>) : AsyncResult<'a, Exception> =  
    async {
        let! res = func ()  |> Async.AwaitTask |> Async.Catch
        return 
            match res with
            | Choice1Of2 r -> Ok r
            | Choice2Of2 e -> Error e
    } |> toAsyncResultAwait


type AsyncResult<'a, 'e> with
    static member bind (f: 'a -> AsyncResult<'b, 'e>) (ar: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> = 
        async {
            let! x = ar.value
            let x = match x with
                            | Ok ok -> f ok
                            | Error err ->  Error err |> toAsyncResult
            return! x.value
        } |> toAsyncResultAwait
    
    static member (>>=) ((ar: AsyncResult<'a, 'e>), (f: 'a -> AsyncResult<'b, 'e>)) = 
        ar |> (fun x ->  AsyncResult.bind f x)
