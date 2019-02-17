module ChiGheDocBao.ViewArticle.Presenter

open System
open ChiGheDocBao
open Common.Domain
open Domain

type ArticleView =
    abstract ShowLoading : message:string -> Async<Async<unit>>
    abstract ShowError : title:string -> content:string -> Async<unit>
    abstract Back : unit -> unit
    abstract OnArticleFetched : unit -> unit
    abstract SetTitle : string -> unit

type CellViewModel =
    | Title of string
    | Description of string
    | Para of string
    | SecImage of string
    | Subtitle of string

[<AllowNullLiteral>]
type ArticlePresenter (articleHead : ArticleHead,
                       downloadArticle : FetchArticle,
                       view : ArticleView) =
    let mutable article : Article option = None

    do view.SetTitle articleHead.Title

    do Async.Start <| async {
        let! hideLoading = view.ShowLoading "Chi ghẻ đang quậy, vui lòng chờ tí"
        let! articleResult = downloadArticle articleHead.Link
        do! hideLoading
        match articleResult with
        | Error msg ->
            do! view.ShowError "Có lỗi xảy ra" msg
            view.Back ()
        | Ok a ->
            article <- Some a
            view.OnArticleFetched ()
    }

    member this.GetCellCount () =
        match article with
        | None -> 0
        | Some a ->
            1 // title
            + 1 // description
            + a.Sections.Length

    member this.GetCell index =
        let a = article.Value
        match index with
        | 0 -> Title articleHead.Title
        | 1 -> Description <| String.Format ("{0} | {1}", articleHead.DateTime.ToString "d/M/yyyy HH:mm", articleHead.Description)
        | _ ->
            match a.Sections.[index - 2] with
            | Section.Para x -> Para x
            | Section.SecImage (url, caption) -> SecImage (caption)
            | Section.Subtitle x -> Subtitle x
