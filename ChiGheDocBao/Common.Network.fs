module ChiGheDocBao.Common.Network

open System
open FSharp.Data
open ChiGheDocBao
open Common.Domain

let downloadString : DownloadString = fun (Url url) ->
    try
        Ok <| Http.RequestString (url, timeout = 10000)
    with ex ->
        Error <| String.Format ("Could not download string [{0}] due to: {1}", url, ex.Message)

let downloadImage : DownloadImage = fun (Url url) ->
    try
        let data = Http.Request (url, timeout = 30000)
        match data.Body with
        | Text _ -> Error <| String.Format ("Not an image: {0}", url)
        | Binary bytes -> Ok <| Image bytes
    with ex ->
        Error <| String.Format ("Could not download image [{0}] due to: {1}", url, ex.Message)
