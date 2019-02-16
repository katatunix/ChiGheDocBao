namespace ChiGheDocBao.ViewArticle

open ChiGheDocBao

module tvOS =
    open System
    open Foundation
    open UIKit

    let test () =

        let t = Type.GetType "WebKit.WKWebView"
        let webview = Activator.CreateInstance(t) :?> WebKit.WKWebView
        let x = WebKit.WKWebView ()
        ()

    //[<Register ("CategoryContentView")>]
    //type tvOSCategoryContentView (handle : IntPtr) =
        //inherit UITableViewController (handle)

//module Domain =
    //open Common.Domain

    //type Section =
    //    | Para of string
    //    | Subtitle of string
    //    | Image of Url * string

    //type Article = {
    //    Title : string
    //    Description : string
    //    Sections : Section list }

    //type DownloadArticle = Url -> (Result<Article, string> -> unit) -> unit

    //// Impl

    //open FSharp.Data

    //type HtmlType = HtmlProvider<"types/article.html">

    //let private getExactlyOne (doc : HtmlType) cssSelector =
    //    try doc.Html.CssSelect cssSelector |> Seq.exactlyOne |> Ok
    //    with ex -> Error ("Could not get css selector: " + cssSelector)

    //let private parseTitle (doc : HtmlType) =
    //    try
    //        let titleNode = doc.Html.CssSelect "head > title" |> Seq.exactlyOne
    //        titleNode.InnerText () |> Ok
    //    with ex ->
    //        Error ("Could not parse title: " + ex.Message)

    //let private parseDescription (doc : HtmlType) =
    //    try
    //        let metaNode = doc.Html.CssSelect "head > meta[name=description]" |> Seq.exactlyOne
    //        metaNode.AttributeValue "content" |> Ok
    //    with ex ->
    //        Error ("Could not parse description: " + ex.Message)

    //let downloadArticle (downloadString : DownloadString) : DownloadArticle =
        //fun articleUrl onDone ->
            //articleUrl
            //|> Parallel.startThread
                //(fun url -> result {
                //    let! htmlString = downloadString url

                //    let! doc = try HtmlType.Parse htmlString |> Ok with ex -> Error ex.Message

                //    let! title = parseTitle doc
                //    let! description = parseDescription doc

                //    let! articleNode = getExactlyOne doc "article.content_detail"

                //    let sections = [
                //        for node in articleNode.Elements () do
                //            if node.Name () = "p" && node.HasClass "Normal" then
                //                yield Para <| node.InnerText()
                //            elif node.Name () = "p" && node.HasClass "subtitle" then
                //                yield Subtitle <| node.InnerText()
                //            elif node.Name () = "table" && node.HasClass "tplCaption" then
                //                let imgNodes = node.CssSelect "img"
                //                if not imgNodes.IsEmpty then
                //                    let imgNode = imgNodes |> List.head
                //                    let imgUrl = imgNode.AttributeValue "src"
                //                    let alt = imgNode.AttributeValue "alt"
                //                    yield Section.Image (Url imgUrl, alt)
                //    ]
                //    return {
                //        Title = title
                //        Description = description
                //        Sections = sections } })
                //onDone
