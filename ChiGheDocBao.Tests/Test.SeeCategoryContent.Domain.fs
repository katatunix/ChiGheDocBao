module ChiGheDocBao.Tests.ViewCategory.Domain

open NUnit.Framework
open FsUnit
open ChiGheDocBao
open Common.Domain
open SeeCategoryContent.Domain

[<Test>]
let ``test fetchArticleHeads`` () =
    let result =
        fetchArticleHeads Common.Network.fetchString (Url "https://vnexpress.net/rss/tin-moi-nhat.rss")
        |> Async.RunSynchronously
    match result with
    | Error message -> failwith message
    | Ok ahs -> printfn "%A" ahs
