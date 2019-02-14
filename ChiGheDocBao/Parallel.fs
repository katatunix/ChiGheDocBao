module Parallel

open System.Threading

let startThread f onDone x =
    let thread = Thread (ThreadStart (fun _ -> f x |> onDone))
    thread.Start ()

let processMulti processItem onDoneEach (items : seq<'T>) =
    let enum = items.GetEnumerator ()
    let mutable index = 0
    let mutable cancel = false

    let next () = lock enum <| fun _ ->
        if enum.MoveNext () && not cancel then
            let curIndex = index
            index <- index + 1
            Some (curIndex, enum.Current)
        else
            None
            
    let threadWork () =
        let rec loop () =
            match next () with
            | None -> ()
            | Some (index, item) ->
                let result = processItem item
                if not cancel then
                    lock items (fun _ -> onDoneEach index result)
                    loop ()
        loop ()

    let jobs = System.Environment.ProcessorCount
    let threads = Array.init jobs (fun _ -> Thread (ThreadStart threadWork))
    for thread in threads do thread.Start ()
    fun () ->
        cancel <- true
        