﻿module ChiGheDocBao.ViewArticle.Domain
open ChiGheDocBao

open Common.Domain

type Section =
    | Para of string
    | Subtitle of string
    | SecImage of Url * string

type Article = {
    Sections : Section []
}

type FetchArticle = Url -> AsyncResult<Article, string>

type FetchSecImages = Section [] -> Stream<int * Image>

// Impl

open FSharp.Data

type private HtmlDoc = HtmlProvider<"<html></html>">

let private parseHtmlDoc htmlString : AsyncResult<HtmlDoc, string> =
    try
        Ok <| HtmlDoc.Parse htmlString
    with ex ->
        Error ex.Message
    |> AsyncResult.ofResult

let private getExactlyOne (doc : HtmlDoc) cssSelector =
    doc.Html.CssSelect cssSelector
    |> List.tryExactlyOne
    |> Result.ofOption ("Could not get exactly one: " + cssSelector)
    |> AsyncResult.ofResult

let private txt (node : HtmlNode) = node.InnerText().Trim()
let private attr name (node : HtmlNode) = (node.AttributeValue name).Trim()

let private (|NormalArticle|SlideshowArticle|) (article : HtmlNode) =
    let nodes = article.CssSelect "div#article_content"
    if nodes.IsEmpty then
        NormalArticle article
    else
        SlideshowArticle nodes.[0]
        
let private parseNormalArticle (article : HtmlNode) = [|
    for node in article.Elements () do
        if node.Name () = "p" && node.HasClass "Normal" then
            yield Para <| txt node
        elif node.Name () = "p" && node.HasClass "subtitle" then
            yield Subtitle <| txt node
        elif node.Name () = "table" && node.HasClass "tplCaption" then
            let secImage =
                node.CssSelect "img"
                |> List.tryHead
                |> Option.map (fun img ->
                    let imgUrl = attr "src" img
                    let caption = attr "alt" img
                    SecImage (Url imgUrl, caption)
                )
            if secImage.IsSome then
                yield secImage.Value
|]

let private parseSlideshowArticle (article : HtmlNode) = [|
    for node in article.Elements () do
        if node.Name () = "div" && node.HasClass "item_slide_show" then
            let imgUrl =
                node.CssSelect ".block_thumb_slide_show > img"
                |> List.tryHead
                |> Option.map (attr "data-original")
            let caption =
                node.CssSelect ".desc_cation"
                |> List.tryHead
                |> Option.map txt
            match imgUrl, caption with
            | Some u, Some c -> yield SecImage (Url u, c)
            | Some u, None -> yield SecImage (Url u, "")
            | _ -> ()
        elif node.Name () = "div" && node.HasClass "fck_detail" then
            yield Para <| txt node
|]

let private parseSections article =
    match article with
    | NormalArticle node -> parseNormalArticle node
    | SlideshowArticle node -> parseSlideshowArticle node

let fetchArticle (fetchString : FetchString) : FetchArticle =
    fun articleUrl -> asyncResult {
        let! htmlString = fetchString articleUrl
        let! doc = parseHtmlDoc htmlString

        let! article = getExactlyOne doc "article.content_detail"

        let sections = parseSections article

        return { Sections = sections }
    }

let fetchSecImages (fetchImage : FetchImage) : FetchSecImages =
    fun sections ->
        let hardFetchImage = fetchImage |> Utils.hard 3

        let imageUrls =
            sections
            |> Seq.mapi (fun i section -> i, section)
            |> Seq.choose (fun (i, section) ->
                match section with
                | SecImage (url, _) -> Some (i, url)
                | _ -> None
            )

        let stream =
            imageUrls
            |> Stream.create 4 (fun (i, url) -> hardFetchImage url)

        let imageObservable =
            stream.Observable
            |> Observable.choose (fun (_, (i, _), imageResult) ->
                match imageResult with
                | Ok image -> Some (i, image)
                | Error _ -> None
            )

        stream
        |> Stream.changeObservable imageObservable
