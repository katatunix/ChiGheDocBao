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
    abstract OnAhsFetched : unit -> unit
    abstract OnThumbnailFetched : int -> unit
    abstract PushArticleView : ArticleHead -> unit

[<AllowNullLiteral>]
type CategoryPresenter (categoryUrl : Url,
                        fetchArticleHeads : FetchArticleHeads,
                        fetchThumbnails : FetchThumbnails,
                        view : CategoryView) =
    let mutable articleHeads : ArticleHead [] = Array.empty
    let thumbnails = ConcurrentDictionary<int, Image> ()
    let mutable stopFetchThumbnails = id

    do Async.Start <| async {
        let! hideLoading = view.ShowLoading "Chi ghẻ đang quậy, vui lòng chờ tí"
        let! ahsResult = fetchArticleHeads categoryUrl
        do! hideLoading
        match ahsResult with
        | Error msg ->
            do! view.ShowError "Có lỗi xảy ra" msg
            view.Back ()
        | Ok ahs ->
            articleHeads <- ahs
            view.OnAhsFetched ()
            let stream = fetchThumbnails ahs
            let dis =
                stream.Observable
                |> Observable.subscribe (fun (index, image) ->
                    thumbnails.[index] <- image
                    view.OnThumbnailFetched index
                )
            stopFetchThumbnails <- stream.Stop >> dis.Dispose
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

    member this.OnArticleHeadSelected index =
        view.PushArticleView articleHeads.[index]

    member this.OnBack () =
        stopFetchThumbnails ()
