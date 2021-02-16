namespace global

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent

// ======================================================================================================
// Misc functions
// ======================================================================================================

[<AutoOpen>]
module Misc =

    let isNotNull x = x |> isNull |> not

    let safe f x = try f x |> Ok with ex -> ex.Message |> Error

    let silent f = try f () with _ -> ()

    let WTF () = failwith "Should not go here!"

    let parseInt (str: string) = match Int32.TryParse str with | true, x -> Some x | _ -> None
    let (| Int | _ |) = parseInt

    let parseFloat (str: string) = match Double.TryParse str with | true, x -> Some x | _ -> None
    let (| Float | _ |) = parseFloat

    let option2string = Option.defaultValue ""
    let string2option s = if String.IsNullOrEmpty s then None else Some s

    let safeAvgBy projection xs = if xs |> Array.length = 0 then 0.0 else xs |> Array.averageBy projection

    let ( /! ) (a: int) (b: int) = if b = 0 then float a else float a / float b

    let removeElement index =
        Seq.indexed
        >> Seq.choose (fun (i, x) -> if i = index then None else Some x)

    let smartConcat separator xs =
        xs |> Seq.filter (fun x -> String.IsNullOrEmpty x |> not) |> String.concat separator

    let hard times f x =
        let rec loop count =
            let y = f x
            match y with
            | Ok _
            | Error _ when count = times -> y
            | _ -> loop (count + 1)
        loop 1

    let memorize f =
        let cache = ConcurrentDictionary<_, _> ()
        fun a ->
            match cache.TryGetValue a with
            | true, b -> Ok b
            | _ ->
                let bResult = f a
                match bResult with
                | Ok b -> cache.[a] <- b
                | Error _ -> ()
                bResult

    type SafeValue<'T> (initialValue : 'T) =
        let mutable value = initialValue
        member this.Value
            with get () = lock this (fun _ -> value)
            and set v = lock this (fun _ -> value <- v)

// ======================================================================================================
// Option computation expression
// ======================================================================================================

[<AutoOpen>]
module OptionComputationExpression =

    type OptionBuilder () =
        member __.Return(x) = Some x
        member __.Bind(x, f) = Option.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let maybe = new OptionBuilder ()

// ======================================================================================================
// Functions for Result type
// ======================================================================================================

[<RequireQualifiedAccess>]
module Result =

    let ofOption errorValue = function
        | Some v -> Ok v
        | None -> Error errorValue

    let toOption = function
        | Ok v -> Some v
        | Error _ -> None

    let isOk = function 
        | Ok _ -> true
        | Error _ -> false
        
    let isError xR = 
        xR |> isOk |> not

    let forceOk = function
        | Ok x -> x
        | Error _ -> raise (Exception())

    let forceError = function
        | Ok _ -> raise (Exception())
        | Error x -> x

// ======================================================================================================
// Result computation expression
// ======================================================================================================

[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder () =
        member __.Return(x) = Ok x
        member __.Bind(x, f) = Result.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let trial = new ResultBuilder ()

// ======================================================================================================
// Functions for Async type
// ======================================================================================================

[<RequireQualifiedAccess>]
module Async =

    let retn x = async.Return x

    let bind f xA = async.Bind (xA, f)

    let map f xA = async { let! x = xA in return f x }

    let start ct work = Async.Start (work, ct)

    let safe work = async {
        try
            let! x = work
            return Some x
        with _ ->
            return None
    }

    /// Creates an asynchronous workflow that non-deterministically returns the
    /// result of one of the two specified workflows (the one that completes first).
    /// This is similar to Task.WhenAny.
    /// Source: https://github.com/tpetricek/TryJoinads/blob/master/src/FSharp.Joinads/Async.fs
    let whenAny works =
        Async.FromContinuations <| fun (cont, econt, ccont) ->
            // Results from the two
            let results = Array.map (fun _ -> Choice1Of3()) works
            let handled = ref false
            let lockObj = obj ()
            let synchronized f = lock lockObj f

            // Called when one of the workflows completes
            let complete () =
                let op =
                    synchronized <| fun _ ->
                        // If we already handled result (and called continuation)
                        // then ignore. Otherwise, if the computation succeeds, then
                        // run the continuation and mark state as handled.
                        // Only throw if all workflows failed.
                        if !handled then ignore
                        else
                            let succ = Seq.tryPick (function Choice2Of3 v -> Some v | _ -> None) results
                            match succ with
                            | Some value -> handled := true; (fun _ -> cont value)
                            | _ ->
                                if Seq.forall (function Choice3Of3 _ -> true | _ -> false) results then
                                    let exs = Array.map (function Choice3Of3 ex -> ex | _ -> WTF()) results
                                    (fun _ -> econt (AggregateException(exs)))
                                else ignore
                // Actually run the continuation
                // (this shouldn't be done in the lock)
                op ()

            // Run a workflow and write result (or exception to a ref cell)
            let run index workflow = async {
                try
                    let! res = workflow
                    synchronized (fun _ -> results.[index] <- Choice2Of3 res)
                with e -> 
                    synchronized (fun _ -> results.[index] <- Choice3Of3 e)
                complete ()
            }

            // Start all work items - using StartImmediate, because it
            // should be started on the current synchronization context
            works |> Seq.iteri (fun index work -> Async.StartImmediate <| run index work)

    let withTimeout (timeMs:int) work =
        let success = async {
            let! x = work
            return Some x
        }
        let timeout = async {
            do! Async.Sleep timeMs
            return None
        }
        whenAny [| success; timeout |]

    let withTimein timeMs work = async {
        let beginTime = DateTime.Now
        let! x = work
        let endTime = DateTime.Now
        let duration = (endTime - beginTime).TotalMilliseconds |> int
        if duration < timeMs then do! Async.Sleep (timeMs - duration)
        return x
    }

// ======================================================================================================
// AsyncResult
// ======================================================================================================

type AsyncResult<'Success,'Failure> = 
    Async<Result<'Success,'Failure>>

[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `AsyncResult.xxx` prefix to be used
module AsyncResult =

    /// Lift a function to AsyncResult
    let map f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        Async.map (Result.map f) x

    /// Lift a function to AsyncResult
    let mapError f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        Async.map (Result.mapError f) x

    /// Apply ignore to the internal value
    let ignore x = 
        x |> map ignore    

    /// Lift a value to AsyncResult
    let retn x : AsyncResult<_,_> = 
        x |> Result.Ok |> Async.retn

    /// Handles asynchronous exceptions and maps them into Failure cases using the provided function
    let catch f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        x
        |> Async.Catch
        |> Async.map(function
            | Choice1Of2 (Ok v) -> Ok v
            | Choice1Of2 (Error err) -> Error err
            | Choice2Of2 ex -> Error (f ex))

    /// Apply a monadic function to an AsyncResult value  
    let bind (f: 'a -> AsyncResult<'b,'c>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> = async {
        let! xResult = xAsyncResult 
        match xResult with
        | Ok x -> return! f x
        | Error err -> return (Error err)
    }

    //-----------------------------------
    // Converting between AsyncResults and other types

    /// Lift a value into an Ok inside a AsyncResult
    let ofSuccess x : AsyncResult<_,_> = 
        x |> Result.Ok |> Async.retn 

    /// Lift a value into an Error inside a AsyncResult
    let ofError x : AsyncResult<_,_> = 
        x |> Result.Error |> Async.retn 

    /// Lift a Result into an AsyncResult
    let ofResult x : AsyncResult<_,_> = 
        x |> Async.retn

    /// Lift a Async into an AsyncResult
    let ofAsync x : AsyncResult<_,_> = 
        x |> Async.map Result.Ok

    //-----------------------------------
    // Utilities lifted from Async

    let sleep (ms:int) = 
        Async.Sleep ms |> ofAsync

// ======================================================================================================
// AsyncResult computation expression
// ======================================================================================================
    
[<AutoOpen>]
module AsyncResultComputationExpression = 

    type AsyncResultBuilder() = 
        member __.Return(x) = AsyncResult.retn x
        member __.Bind(x, f) = AsyncResult.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let asyncResult = AsyncResultBuilder ()

// ======================================================================================================
// AsyncOption
// ======================================================================================================

type AsyncOption<'T> = 
    Async<Option<'T>>

[<RequireQualifiedAccess>]
module AsyncOption =

    let map f (x: AsyncOption<_>) : AsyncOption<_> =
        Async.map (Option.map f) x

    let map2 f (x: AsyncOption<_>) : AsyncOption<_> =
        Async.map (Option.bind f) x

    let bind (f: 'a -> AsyncOption<'b>) (xAsyncOption : AsyncOption<_>) : AsyncOption<_> = async {
        let! xOpt = xAsyncOption
        match xOpt with
        | Some x -> return! f x
        | None -> return None
    }

    let ignore x = 
        x |> map ignore    

    let retn x : AsyncOption<_> = 
        x |> Some |> Async.retn
        
// ======================================================================================================
// AsyncResult computation expression
// ======================================================================================================
    
[<AutoOpen>]
module AsyncOptionComputationExpression = 

    type AsyncOptionBuilder() = 
        member __.Return(x) = AsyncOption.retn x
        member __.Bind(x, f) = AsyncOption.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let asyncOption = AsyncOptionBuilder ()

// ======================================================================================================
// Parallel util functions
// ======================================================================================================

module Parallel =

    let start jobs (items : seq<_>) map callback =
        let enum = items.GetEnumerator ()
        let mutable index = 0
        let still = SafeValue true

        let next () = lock enum <| fun _ ->
            if still.Value && enum.MoveNext () then
                let i = index
                index <- index + 1
                Some (i, enum.Current)
            else
                None

        let threadWork () =
            let rec loop () =
                match next () with
                | None -> ()
                | Some (index, item) ->
                    let result = map item
                    if still.Value then
                        callback index item result
                        loop ()
            loop ()

        let threads = Array.init jobs (fun _ -> Thread (ThreadStart threadWork))
        for thread in threads do thread.Start ()
        fun () -> still.Value <- false

    let startDefaultJobs items map callback =
        start System.Environment.ProcessorCount items map callback

    let startTask (task: Task<_>) =
        task |> Async.AwaitTask |> Async.Ignore |> Async.Start

    type Gate (n: int) =
        let semaphore = new Semaphore (n, n)
        member this.AsyncAcquire (?timeout) = async {
            match! Async.AwaitWaitHandle (semaphore, ?millisecondsTimeout = timeout) with
            | true ->
                return { new System.IDisposable with
                            member this.Dispose () = semaphore.Release () |> ignore }
            | false ->
                return! failwith "Could not acquire a semaphore."
        }
        
// ======================================================================================================
// Simple types
// ======================================================================================================

type RString = private RString of string with
    member this.Value = let (RString x) = this in x
    static member Create subject (x: string) =
        if String.IsNullOrEmpty x
        then Error <| sprintf "%s could not be empty." subject
        else Ok <| RString x

type Port = private Port of int with
    member this.Value = let (Port x) = this in x
    static member Create x =
        if x <= 0 || x > 65535
        then Error "Port must be in range 1..65535"
        else Ok <| Port x
    static member Create s =
        if String.IsNullOrEmpty s
        then Error "Port could not be empty."
        else match s with
             | Int x -> Port.Create x
             | _ -> Error "Port must be an integer number."
