module ChiGheDocBao.ViewCategory.Domain

open ChiGheDocBao
open Common.Domain

type FetchArticleHeads = Url -> AsyncResult<ArticleHead [], string>

type FetchThumbnails = ArticleHead [] -> Stream<int * Image>

// Impl

open FSharp.Data
open System.Text.RegularExpressions

type private XmlDoc = XmlProvider<"types/tin-moi-nhat.rss">

let private parseXml xmlString : Result<XmlDoc.Rss, string> =
    try
        Ok <| XmlDoc.Parse xmlString
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

let private parseArticleHead (item : XmlDoc.Item) =
    result {
        let! imageUrl, description = parseDescription item.Description
        return {
            Title = item.Title
            DateTime = item.PubDate.DateTime
            Description = description
            ImageUrl = imageUrl
            Link = Url item.Link
        }
    }

let private parseArticleHeads (rss : XmlDoc.Rss) =
    rss.Channel.Items
    |> Array.map parseArticleHead
    |> Array.choose Result.toOption

let fetchArticleHeads (fetchString : FetchString) : FetchArticleHeads =
    fun categoryUrl ->
        asyncResult {
            let! xmlString = fetchString categoryUrl
            let! rss = parseXml xmlString |> AsyncResult.ofResult
            return! parseArticleHeads rss |> AsyncResult.ofSuccess
        }

let fetchThumbnails (fetchImage : FetchImage) : FetchThumbnails =
    fun articleHeads ->
        let hardFetchImage = fetchImage |> Utils.hard 3
        let stream =
            articleHeads
            |> Stream.create 4 (fun ah -> hardFetchImage ah.ImageUrl)
        let imageObservable =
            stream.Observable
            |> Observable.choose (fun (index, _, imageResult) ->
                match imageResult with
                | Ok image -> Some (index, image)
                | Error _ -> None
            )
        stream
        |> Stream.changeObservable imageObservable
