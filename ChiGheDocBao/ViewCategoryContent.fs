namespace ChiGheDocBao.ViewCategoryContent

open ChiGheDocBao

module Domain =
    open System
    open Common.Domain

    type ArticleHead = {
        Title : string
        DateTime : DateTime
        Description : string
        ImageUrl : Url }

    type DownloadArticleHeads = Url -> (Result<ArticleHead [], string> -> unit) -> unit

    type DownloadThumbnails = ArticleHead [] -> (int -> Image -> unit) -> (unit -> unit)

    // Impl
    
    open FSharp.Data

    type private XmlType = XmlProvider<"""<?xml version="1.0" encoding="UTF-8"?>
        <rss version="2.0" xmlns:slash="http://purl.org/rss/1.0/modules/slash/">
          <channel>
            <title>Tin mới nhất - VnExpress RSS</title>
            <description>VnExpress RSS</description>
            <image>
              <url>https://s.vnecdn.net/vnexpress/i/v20/logos/vne_logo_rss.png</url>
              <title>Tin nhanh VnExpress - Đọc báo, tin tức online 24h</title>
              <link>https://vnexpress.net</link>
            </image>
            <pubDate>Fri, 08 Feb 2019 16:56:21 +0700</pubDate>
            <generator>VnExpress</generator>
            <link>https://vnexpress.net/rss/tin-moi-nhat.rss</link>
            <item>
              <title>Trai làng hò hét dưới nắng rước hai quả pháo dài 6m</title>
              <description><![CDATA[<a href="https://video.vnexpress.net/tin-tuc/thoi-su/trai-lang-ho-het-duoi-nang-ruoc-hai-qua-phao-dai-6m-3878858.html"><img width=130 height=100 src="https://i-vnexpress.vnecdn.net/2019/02/08/515081415431856661854617687951-1454-4903-1549616996_180x108.jpg" ></a></br>Hai quả pháo dài 5 - 6m được thanh niên trai tráng khỏe mạnh rước xung quang làng Đồng Kỵ, Từ Sơn (Bắc Ninh).]]></description>
              <pubDate>Fri, 08 Feb 2019 16:09:56 +0700</pubDate>
              <link>https://video.vnexpress.net/tin-tuc/thoi-su/trai-lang-ho-het-duoi-nang-ruoc-hai-qua-phao-dai-6m-3878858.html</link>
              <guid>https://video.vnexpress.net/tin-tuc/thoi-su/trai-lang-ho-het-duoi-nang-ruoc-hai-qua-phao-dai-6m-3878858.html</guid>
              <slash:comments>0</slash:comments>
            </item>
            <item>
              <title>Phong tục đón Tết của người Triều Tiên</title>
              <description><![CDATA[<a href="https://vnexpress.net/the-gioi/phong-tuc-don-tet-cua-nguoi-trieu-tien-3878866.html"><img width=130 height=100 src="https://i-vnexpress.vnecdn.net/2019/02/08/3-1549616779-6056-1549616899_180x108.jpg" ></a></br>Các gia đình Triều Tiên quây quần trong ngày đầu tiên của năm mới âm lịch, chúc tụng và dùng bữa ăn truyền thống.]]></description>
              <pubDate>Fri, 08 Feb 2019 16:08:54 +0700</pubDate>
              <link>https://vnexpress.net/the-gioi/phong-tuc-don-tet-cua-nguoi-trieu-tien-3878866.html</link>
              <guid>https://vnexpress.net/the-gioi/phong-tuc-don-tet-cua-nguoi-trieu-tien-3878866.html</guid>
              <slash:comments>0</slash:comments>
            </item>
          </channel>
        </rss>""">

    let private parseXml xmlString : Result<XmlType.Rss, string> =
        try
            Ok (XmlType.Parse xmlString)
        with ex ->
            Error ("Invalid XML: " + ex.Message)

    let private parseDescription description =
        let pattern = """src="(.+)" ></a></br>(.+)"""
        let m = Text.RegularExpressions.Regex.Match (description, pattern)
        if m.Success then
            let imageUrl = m.Groups.[1].Value
            let content = m.Groups.[2].Value
            Ok (Url imageUrl, content)
        else
            Error ("Invalid description: " + description)

    let private parseArticleHead (item : XmlType.Item) =
        result {
            let! imageUrl, description = parseDescription item.Description
            return {
                Title = item.Title
                DateTime = item.PubDate.DateTime
                Description = description
                ImageUrl = imageUrl } }

    let private parseArticleHeads (rss : XmlType.Rss) =
        rss.Channel.Items
        |> Array.map parseArticleHead
        |> Array.choose Result.toOption

    let downloadArticleHeads (downloadString : DownloadString) : DownloadArticleHeads =
        fun categoryUrl onDone ->
            categoryUrl
            |> Parallel.startThread
                (fun url ->
                    result {
                        let! xmlString = downloadString url
                        let! rss = parseXml xmlString
                        return parseArticleHeads rss })
                onDone

    let downloadThumbnails (downloadImage : DownloadImage) : DownloadThumbnails =
        let hardDownloadImage = downloadImage |> Utils.hard 3
        fun articleHeads onDoneEach ->
            articleHeads
            |> Parallel.processMulti
                (fun articleHead -> hardDownloadImage articleHead.ImageUrl)
                (fun index result ->
                    match result with
                    | Ok image -> onDoneEach index image
                    | Error _ -> ())

module Presenter =
    open System
    open Common.Domain
    open Domain

    type ArticleHeadViewModel = {
        Title : string
        Description : string
        Image : Image option }

    type CategoryContentView =
        abstract ShowLoadingMessage : string -> ((unit -> unit) -> unit)
        abstract ShowErrorMessage : string -> string -> (unit -> unit) -> unit
        abstract Back : unit -> unit
        abstract OnListUpdated : unit -> unit
        abstract OnThumbnailUpdated : unit -> unit

    [<AllowNullLiteral>]
    type CategoryContentPresenter (categoryUrl : Url,
                                   downloadArticleHeads : DownloadArticleHeads,
                                   downloadThumbnails : DownloadThumbnails,
                                   view : CategoryContentView,
                                   runOnViewThread) =

        let hideLoadingMessage = view.ShowLoadingMessage "Chi ghẻ đang quậy, vui lòng chờ tí"

        let mutable articleHeads : ArticleHead [] = Array.empty
        let thumbnails = Collections.Generic.Dictionary<int, Image> ()
        let mutable stopDownloadThumbnails = id

        do downloadArticleHeads categoryUrl <| fun result ->
            hideLoadingMessage <| fun _ ->
                match result with
                | Error message ->
                    view.ShowErrorMessage "Có lỗi xảy ra" message view.Back
                | Ok heads ->
                    articleHeads <- heads
                    view.OnListUpdated ()
                    stopDownloadThumbnails <-
                        downloadThumbnails
                            articleHeads
                            <| fun index image -> runOnViewThread <| fun _ -> thumbnails.[index] <- image; view.OnThumbnailUpdated ()

        member this.GetArticleHeadsCount () =
            articleHeads.Length

        member this.GetArticleHead index =
            let article = articleHeads.[index]
            let vm : ArticleHeadViewModel = {
                Title = article.Title
                Description = String.Format ("{0} | {1}", article.DateTime.ToString "d/M/yyyy HH:mm", article.Description)
                Image = match thumbnails.TryGetValue index with true, x -> Some x | _ -> None }
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
        let mutable presenter : CategoryContentPresenter = null

        member this.Init cat =
            category <- cat

        override this.ViewDidLoad () =
            base.ViewDidLoad ()

            this.NavigationItem.Title <- category.Name
            presenter <- CategoryContentPresenter (category.Url,
                                                   downloadArticleHeads Common.Network.downloadString,
                                                   downloadThumbnails cachedDownloadImage,
                                                   this,
                                                   this.InvokeOnMainThread)

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

        interface CategoryContentView with
            member this.ShowLoadingMessage message =
                showToast this message

            member this.ShowErrorMessage title message completion =
                showMessageBox this title message completion

            member this.Back () =
                this.NavigationController.PopViewController false |> ignore

            member this.OnListUpdated () =
                this.TableView.ReloadData ()

            member this.OnThumbnailUpdated () =
                for cell in this.TableView.VisibleCells do
                    this.UpdateCell (cell :?> ArticleHeadCell) |> ignore
