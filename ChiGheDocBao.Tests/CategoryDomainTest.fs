module ChiGheDocBao.CategoryDomainTest

open NUnit.Framework
open FsUnit
open ChiGheDocBao
open Common.Domain
open Category.Domain

[<Test>]
let ``test fetchArticleHeads`` () =
    let result =
        fetchArticleHeads Common.Network.fetchString (Url "https://vnexpress.net/rss/tin-moi-nhat.rss")
        |> Async.RunSynchronously
    match result with
    | Error message -> failwith message
    | Ok ahs -> printfn "%A" ahs
