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

let memo (f : 'T -> Result<'U, 'V>) =
    let cache = ConcurrentDictionary<'T, 'U> ()
    fun t ->
        match cache.TryGetValue t with
        | true, u -> Ok u
        | _ ->
            let result = f t
            match result with
            | Ok u -> cache.[t] <- u
            | Error _ -> ()
            result
            