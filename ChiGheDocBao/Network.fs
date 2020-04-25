module ChiGheDocBao.Network

open System
open FSharp.Data
open ChiGheDocBao

let fetchString : FetchString = fun (Url url) ->
    async {
        try
            let! str = Http.AsyncRequestString (url, timeout = 10000, responseEncodingOverride = "UTF-8")
            return Ok str
        with ex ->
            return Error ex.Message
    }

let fetchImage : FetchImage = fun (Url url) ->
    try
        let data = Http.Request (url, timeout = 30000)
        match data.Body with
        | Text _ -> Error <| String.Format ("Not an image: {0}", url)
        | Binary bytes -> Ok <| Image bytes
    with ex ->
        Error ex.Message

let fetchImage_Cached : FetchImage = memorize fetchImage
