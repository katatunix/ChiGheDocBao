module ChiGheDocBao.ViewArticle.Domain
open ChiGheDocBao

open Common.Domain

type NString = NString of string with
    member this.Value = let (NString str) = this in str
    static member Create (str : string) =
        let str = str.Trim()
        if str.Length > 0 then Some (NString str)
        else None

type Section =
    | Para of NString
    | Subtitle of NString
    | SecImage of Url
    | Caption of NString

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

let private getHead (doc : HtmlDoc) cssSelector =
    doc.Html.CssSelect cssSelector
    |> List.tryHead
    |> Result.ofOption ("Could not get: " + cssSelector)
    |> AsyncResult.ofResult

let private innerText (node : HtmlNode) = node.InnerText()
let private attr name (node : HtmlNode) = node.AttributeValue name

let private splitParas (str : string) =
    str.Split ([| "\r\n"; "\n" |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.choose NString.Create

let private (|NormalArticle|SlideshowArticle|) (article : HtmlNode) =
    let nodes = article.CssSelect "div#article_content"
    if nodes.IsEmpty then
        NormalArticle article
    else
        SlideshowArticle nodes.[0]
        
let private parseNormalArticle (article : HtmlNode) = [|
    for node in article.Elements () do

        if node.Name () = "p" && node.HasClass "Normal" then
            yield! innerText node |> splitParas |> Array.map Para

        elif node.Name () = "p" && node.HasClass "subtitle" then
            yield! innerText node |> splitParas |> Array.map Subtitle

        elif node.Name () = "table" && node.HasClass "tplCaption" then
            let imgs = node.CssSelect "img"
            if not imgs.IsEmpty then
                let img = imgs.[0]
                yield SecImage <| Url (attr "src" img)
                //yield! attr "alt" img |> splitParas |> Array.map Caption // NOT A CORRECT WAY
                match node.CssSelect "p.Image" |> List.tryHead with
                | Some node -> yield! innerText node |> splitParas |> Array.map Caption
                | None -> ()
|]

let private parseSlideshowArticle (article : HtmlNode) = [|
    for node in article.Elements () do

        if node.Name () = "div" && node.HasClass "item_slide_show" then
            let imgUrl =
                node.CssSelect ".block_thumb_slide_show > img"
                |> List.tryHead
                |> Option.map (attr "data-original")
            match imgUrl with Some x -> yield SecImage (Url x) | None -> ()

            match node.CssSelect ".desc_cation" |> List.tryHead with
            | Some node ->
                yield! innerText node |> splitParas |> Array.map Caption
            | None -> ()

        elif node.Name () = "div" && node.HasClass "fck_detail" then
            match innerText node |> NString.Create with Some x -> yield Para x | None -> ()
|]

let private parseSections article =
    match article with
    | NormalArticle node -> parseNormalArticle node
    | SlideshowArticle node -> parseSlideshowArticle node

let fetchArticle (fetchString : FetchString) : FetchArticle =
    fun articleUrl -> asyncResult {
        let! htmlString = fetchString articleUrl
        let! doc = parseHtmlDoc htmlString

        let! article =
            getHead doc "article.content_detail"
            |> AsyncResult.mapError (fun _ -> "Chưa hỗ trợ đọc bài viết dạng này")

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
                | SecImage url -> Some (i, url)
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
