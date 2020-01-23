﻿module ChiGheDocBao.Category.Presenter

open System
open System.Collections.Concurrent
open ChiGheDocBao
open Common.Domain
open Domain

type ArticleHeadViewModel = {
    Title : string
    Description : string
    Image : Image option
}

type CategoryContentView =
    abstract ShowLoading : message:string -> Async<Async<unit>>
    abstract ShowError : title:string -> content:string -> Async<unit>
    abstract Back : unit -> unit
    abstract RefreshAllCells : unit -> unit
    abstract RefreshCell : int -> unit
    abstract ShowArticleContent : ArticleHead -> unit

[<AllowNullLiteral>]
type CategoryContentPresenter (category : Category,
                               fetchArticleHeads : FetchArticleHeads,
                               fetchThumbnails : FetchThumbnails,
                               view : CategoryContentView) =

    let mutable articleHeads : ArticleHead [] = Array.empty
    let thumbnails = ConcurrentDictionary<int, Image> ()
    let mutable stopFetchingThumbnails = id

    do Async.Start <| async {
        let! hideLoading = view.ShowLoading "Chi ghẻ đang quậy, vui lòng chờ tí"
        let! ahsResult = fetchArticleHeads category.Url
        do! hideLoading
        match ahsResult with
        | Error msg ->
            do! view.ShowError "Có lỗi xảy ra" msg
            view.Back ()
        | Ok ahs ->
            articleHeads <- ahs
            view.RefreshAllCells ()
            let stream = fetchThumbnails ahs
            let dis =
                stream.Observable
                |> Observable.subscribe (fun (index, image) ->
                    thumbnails.[index] <- image
                    view.RefreshCell index
                )
            stopFetchingThumbnails <- stream.Stop >> dis.Dispose
            stream.Start ()
    }

    member this.Title = category.Name

    member this.Length = articleHeads.Length

    member this.GetCellViewModel index =
        let article = articleHeads.[index]
        let vm : ArticleHeadViewModel = {
            Title = article.Title
            Description = String.Format ("{0} | {1}", article.DateTime.ToString "d/M/yyyy HH:mm", article.Description)
            Image = match thumbnails.TryGetValue index with true, x -> Some x | _ -> None
        }
        vm

    member this.OnCellSelected index =
        view.ShowArticleContent articleHeads.[index]

    member this.OnBack () =
        stopFetchingThumbnails ()