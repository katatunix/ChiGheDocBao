module ChiGheDocBao.ViewCategory.Domain
open ChiGheDocBao

open System
open Common.Domain

type ArticleHead = {
    Title : string
    DateTime : DateTime
    Description : string
    ImageUrl : Url
}

type DownloadArticleHeads = Url -> AsyncResult<ArticleHead [], string>

type DownloadThumbnails = ArticleHead [] -> Stream<int * Image>

// Impl

open FSharp.Data
open System.Text.RegularExpressions

type private XmlType = XmlProvider<"types/tin-moi-nhat.rss">

let private parseXml xmlString : Result<XmlType.Rss, string> =
    try
        Ok <| XmlType.Parse xmlString
    with ex ->
        Error <| "Invalid XML: " + ex.Message

let private parseDescription description =
    let pattern = """src="(.+)" /?></a>(?:</br>)?(.+)"""
    let m = Regex.Match (description, pattern)
    if m.Success then
        let imageUrl = m.Groups.[1].Value
        let content = m.Groups.[2].Value
        Ok (Url imageUrl, content)
    else
        Error <| "Invalid description: " + description

let private parseArticleHead (item : XmlType.Item) =
    result {
        let! imageUrl, description = parseDescription item.Description
        return {
            Title = item.Title
            DateTime = item.PubDate.DateTime
            Description = description
            ImageUrl = imageUrl
        }
    }

let private parseArticleHeads (rss : XmlType.Rss) =
    rss.Channel.Items
    |> Array.map parseArticleHead
    |> Array.choose Result.toOption

let downloadArticleHeads (downloadString : DownloadString) : DownloadArticleHeads =
    fun categoryUrl ->
        asyncResult {
            let! xmlString = downloadString categoryUrl
            let! rss = parseXml xmlString |> AsyncResult.ofResult
            return! parseArticleHeads rss |> AsyncResult.ofSuccess
        }

let downloadThumbnails (downloadImage : DownloadImage) : DownloadThumbnails =
    fun articleHeads ->
        let hardDownloadImage = downloadImage |> Utils.hard 3
        let stream =
            articleHeads
            |> Stream.create (fun ah -> hardDownloadImage ah.ImageUrl)
        let imageObservable =
            stream.Observable
            |> Observable.choose (fun (index, imageResult) ->
                match imageResult with
                | Ok image -> Some (index, image)
                | Error _ -> None
            )
        stream
        |> Stream.changeObservable imageObservable
