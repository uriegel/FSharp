module AsyncResult

type AsyncResult<'a, 'e> = {
    value: Async<Result<'a, 'e>>
}

let toAsyncResultAwait (ar: Async<Result<'a, 'e>>): AsyncResult<'a, 'e> = {value = ar }
let ToAsyncResult (r: Result<'a, 'e>): AsyncResult<'a, 'e> = {value = async { return r } }

type AsyncResult<'a, 'e> with
    static member toResult a =
        a.value

    static member bind (f: 'a -> AsyncResult<'b, 'e>) (ar: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> = 
        async {
            let! x = ar.value
            let x = match x with
                            | Ok ok -> f ok
                            | Error err ->  Error err |> ToAsyncResult
            return! x.value
        } |> toAsyncResultAwait
    
    static member (>>=) ((ar: AsyncResult<'a, 'e>), (f: 'a -> AsyncResult<'b, 'e>)) = 
        ar |> (fun x ->  AsyncResult.bind f x)
