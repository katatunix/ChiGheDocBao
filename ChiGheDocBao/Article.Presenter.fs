﻿module ChiGheDocBao.Article.Presenter

open System
open System.Collections.Concurrent
open ChiGheDocBao
open Common.Domain
open Domain

type ArticleView =
    abstract ShowLoading : message:string -> Async<Async<unit>>
    abstract ShowError : title:string -> content:string -> Async<unit>
    abstract Back : unit -> unit
    abstract RefreshAllCells : unit -> unit
    abstract RefreshCell : int -> unit

type CellViewModel =
    | Title of string
    | Description of string
    | Para of string
    | SecImage of Image option
    | Caption of string
    | Subtitle of string

[<AllowNullLiteral>]
type ArticlePresenter (articleHead : ArticleHead,
                       fetchArticleBody : FetchArticleBody,
                       fetchSecImages : FetchSecImages,
                       view : ArticleView) =
    let mutable articleBody : ArticleBody option = None
    let secImages = ConcurrentDictionary<int, Image> ()
    let mutable stopFetchingSecImages = id

    do Console.WriteLine articleHead.Link.Value

    do Async.Start <| async {
        let! hideLoading = view.ShowLoading "Chi ghẻ đang quậy, vui lòng chờ tí"
        let! articleResult = fetchArticleBody articleHead.Link
        do! hideLoading
        match articleResult with
        | Error msg ->
            do! view.ShowError "Có lỗi xảy ra" msg
            view.Back ()
        | Ok body ->
            articleBody <- Some body
            view.RefreshAllCells ()
            let stream = fetchSecImages body.Sections
            let dis =
                stream.Observable
                |> Observable.subscribe (fun (index, image) ->
                    secImages.[index] <- image
                    view.RefreshCell (index + 2)
                )
            stopFetchingSecImages <- stream.Stop >> dis.Dispose
            stream.Start ()
    }

    member this.Title = articleHead.Title

    member this.Length =
        match articleBody with
        | None -> 0
        | Some body ->
            1 // title
            + 1 // description
            + body.Sections.Length

    member this.GetCellViewModel index =
        let a = articleBody.Value
        match index with
        | 0 -> Title articleHead.Title
        | 1 -> Description <| String.Format ("{0} | {1}", articleHead.DateTime.ToString "d/M/yyyy HH:mm", articleHead.Description)
        | _ ->
            let sectionIndex = index - 2
            match a.Sections.[sectionIndex] with
            | Section.Para str ->
                Para str.Value
            | Section.SecImage url ->
                let imageOpt = match secImages.TryGetValue sectionIndex with true, image -> Some image | _ -> None
                SecImage imageOpt
            | Section.Subtitle str ->
                Subtitle str.Value
            | Section.Caption str ->
                Caption str.Value

    member this.OnBack () =
        stopFetchingSecImages ()
