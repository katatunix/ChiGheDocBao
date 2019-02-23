module Utils

open System.Collections.Concurrent

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
