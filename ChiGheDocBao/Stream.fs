namespace global

open System
open System.Threading
open Utils

type Stream<'T> = {
    Observable : IObservable<'T>
    Start : unit -> unit
    Stop : unit -> unit
}

module Stream =

    let changeObservable obs stream = {
        Observable = obs
        Start = stream.Start
        Stop = stream.Stop
    }

    let create jobs map (items : seq<_>) =
        let enum = items.GetEnumerator ()
        let mutable index = 0
        let still = SafeValue (true)

        let next () = lock enum <| fun _ ->
            if still.Value && enum.MoveNext () then
                let i = index
                index <- index + 1
                Some (i, enum.Current)
            else
                None

        let event = Event<_> ()

        let threadWork () =
            let rec loop () =
                match next () with
                | None -> ()
                | Some (index, item) ->
                    let result = map item
                    if still.Value then
                        event.Trigger (index, result)
                        loop ()
            loop ()
            
        let threads = Array.init jobs (fun _ -> Thread (ThreadStart threadWork))
        {
            Observable = event.Publish
            Start = fun _ -> for thread in threads do thread.Start ()
            Stop = fun _ -> still.Value <- false
        }
