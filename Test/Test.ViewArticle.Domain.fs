module Test.ViewArticle.Domain

open System
open NUnit.Framework
open FsUnit
open ChiGheDocBao
open Common.Domain
open ViewArticle.Domain
open System.Threading

[<Test>]
let test () =
    use mre = new ManualResetEvent (false)
    downloadArticle
        Common.Network.downloadString
        //(Url "https://vnexpress.net/doi-song/17-dau-hieu-cho-thay-som-muon-ban-cung-tro-thanh-trieu-phu-3878849.html")
        (Url "https://vnexpress.net/kinh-doanh/nguoi-sai-gon-chen-lan-mua-heo-vit-quay-cung-than-tai-3881054.html")
        (fun result -> printfn "%A" result; mre.Set () |> ignore)
    mre.WaitOne () |> ignore


