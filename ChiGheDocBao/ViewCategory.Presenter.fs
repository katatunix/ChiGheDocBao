module ChiGheDocBao.ViewCategory.Presenter
open ChiGheDocBao

open System
open System.Collections.Concurrent

open Common.Domain
open Domain

type ArticleHeadViewModel = {
    Title : string
    Description : string
    Image : Image option
}

type CategoryView =
    abstract ShowLoading : message:string -> Async<Async<unit>>
    abstract ShowError : title:string -> content:string -> Async<unit>
    abstract Back : unit -> unit
    abstract OnAhsUpdated : unit -> unit
    abstract OnThumbnailUpdated : int -> unit

[<AllowNullLiteral>]
type CategoryPresenter (categoryUrl : Url,
                        downloadArticleHeads : DownloadArticleHeads,
                        downloadThumbnails : DownloadThumbnails,
                        view : CategoryView) =

    let mutable articleHeads : ArticleHead [] = Array.empty
    let thumbnails = ConcurrentDictionary<int, Image> ()
    let mutable stopDownloadThumbnails = id

    do Async.Start <| async {
        let! hideLoading = view.ShowLoading "Chi ghẻ đang quậy, vui lòng chờ tí"
        let! ahsResult = downloadArticleHeads categoryUrl
        do! hideLoading
        match ahsResult with
        | Error msg ->
            do! view.ShowError "Có lỗi xảy ra" msg
            view.Back ()
        | Ok ahs ->
            articleHeads <- ahs
            view.OnAhsUpdated ()
            let stream = downloadThumbnails ahs
            let dis =
                stream.Observable
                |> Observable.subscribe (fun (index, image) ->
                    thumbnails.[index] <- image
                    view.OnThumbnailUpdated index
                )
            stopDownloadThumbnails <- dis.Dispose >> stream.Stop
            stream.Start ()
    }

    member this.GetArticleHeadsCount () =
        articleHeads.Length

    member this.GetArticleHead index =
        let article = articleHeads.[index]
        let vm : ArticleHeadViewModel = {
            Title = article.Title
            Description = String.Format ("{0} | {1}", article.DateTime.ToString "d/M/yyyy HH:mm", article.Description)
            Image = match thumbnails.TryGetValue index with true, x -> Some x | _ -> None
        }
        vm

    member this.OnBack () =
        stopDownloadThumbnails ()
