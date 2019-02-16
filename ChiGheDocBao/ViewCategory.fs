namespace ChiGheDocBao.ViewCategory

open ChiGheDocBao

module Domain =
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
        let pattern = """src="(.+)" /?></a></br>(.+)"""
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
            stream |> Stream.changeObservable imageObservable

module Presenter =
    open System
    open System.Collections.Concurrent
    open Common.Domain
    open Domain

    type ArticleHeadViewModel = {
        Title : string
        Description : string
        Image : Image option }

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

        let task = async {
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

        do task |> Async.Start

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

module tvOS =
    open System
    open Foundation
    open UIKit
    open Common.Domain
    open Domain
    open Presenter
    open Common.tvOS

    let cachedDownloadImage = Common.Network.downloadImage |> Utils.memo

    [<Register ("ArticleHeadCell")>]
    type ArticleHeadCell (handle : IntPtr) =
        inherit UITableViewCell (handle)
        member val Index : int = 0 with get, set
        [<Outlet>]
        member val TitleLabel : UILabel = null with get, set
        [<Outlet>]
        member val DescriptionLabel : UILabel = null with get, set
        [<Outlet>]
        member val Thumbnail : UIImageView = null with get, set

    [<Register ("CategoryContentView")>]
    type tvOSCategoryContentView (handle : IntPtr) =
        inherit UITableViewController (handle)

        let mutable category = Category.Dummy
        let mutable presenter : CategoryPresenter = null

        member this.Init cat =
            category <- cat

        override this.ViewDidLoad () =
            base.ViewDidLoad ()

            this.NavigationItem.Title <- category.Name
            presenter <- CategoryPresenter (category.Url,
                                            downloadArticleHeads Common.Network.downloadString,
                                            downloadThumbnails cachedDownloadImage,
                                            this)

        override this.RowsInSection (tableView, section) =
            presenter.GetArticleHeadsCount () |> nint

        member private this.UpdateCell (cell : ArticleHeadCell) =
            let vm = presenter.GetArticleHead cell.Index

            cell.TitleLabel.Text <- vm.Title
            cell.DescriptionLabel.Text <- vm.Description

            match vm.Image with
            | Some (Image bytes) ->
                cell.Thumbnail.Image <- UIImage.LoadFromData (NSData.FromArray bytes)
            | None ->
                cell.Thumbnail.Image <- null

            cell.LayoutIfNeeded ()
            cell :> UITableViewCell

        override this.GetCell (tableView, indexPath) =
            let cell = tableView.DequeueReusableCell "ArticleHeadCell" :?> ArticleHeadCell
            cell.Index <- indexPath.Row
            this.UpdateCell cell

        override this.WillMoveToParentViewController vc =
            base.WillMoveToParentViewController vc
            if not (isNull presenter) then
                presenter.OnBack ()

        interface CategoryView with
            member this.ShowLoading message =
                showToast this message

            member this.ShowError title message =
                showAlert this title message

            member this.Back () =
                this.InvokeOnMainThread (fun _ ->
                    this.NavigationController.PopViewController false |> ignore
                )

            member this.OnAhsUpdated () =
                this.InvokeOnMainThread (fun _ -> this.TableView.ReloadData ())

            member this.OnThumbnailUpdated _ =
                this.InvokeOnMainThread (fun _ ->
                    for cell in this.TableView.VisibleCells do
                        this.UpdateCell (cell :?> ArticleHeadCell) |> ignore
                )
