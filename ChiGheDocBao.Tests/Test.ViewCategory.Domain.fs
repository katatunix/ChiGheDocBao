module ChiGheDocBao.Test.ViewCategory.Domain
open ChiGheDocBao

open NUnit.Framework
open FsUnit
open System.IO
open Common.Domain
open ViewCategory.Domain

[<Test>]
let ``test downloadArticleHeads`` () =
    let fakeDownloadString : DownloadString = fun _ ->
        File.ReadAllText "/Users/nghia/Projects/ChiGheDocBao/ChiGheDocBao.Tests/types/tin-moi-nhat.rss"
        |> AsyncResult.ofSuccess
    downloadArticleHeads fakeDownloadString Url.Dummy
    |> Async.RunSynchronously
    |> function
        | Error _ -> raise(exn())
        | Ok ahs ->
            ahs.Length |> should equal 25
            ahs.[0].Title |> should equal "Vietnam designer’s collection at New York Fashion Week garners global attention"
