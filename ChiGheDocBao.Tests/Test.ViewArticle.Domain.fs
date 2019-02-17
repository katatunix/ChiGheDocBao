module ChiGheDocBao.Tests.ViewArticle.Domain
open ChiGheDocBao

open NUnit.Framework
open FsUnit
open Common.Domain
open ViewArticle.Domain

[<Test>]
let ``test`` ()  =
    //Url "https://vnexpress.net/kinh-doanh/4-khuyen-nghi-dau-tu-dia-oc-khong-nen-bo-qua-nam-ky-hoi-3881314.html"
    //Url "https://vnexpress.net/du-lich/quan-ca-phe-trong-biet-thu-co-ten-la-o-da-lat-3869853.html"
    //Url "https://vnexpress.net/thoi-su/cuc-truong-hang-khong-my-kiem-tra-tung-bien-ban-bay-truoc-khi-cap-chung-chi-an-toan-3882128.html"
    Url "https://vnexpress.net/thoi-su/hon-30-hanh-khach-la-het-tren-oto-giuong-nam-lao-vao-nha-dan-3880934.html"
    |> downloadArticle Common.Network.downloadString 
    |> Async.RunSynchronously
    |> printfn "%A"
