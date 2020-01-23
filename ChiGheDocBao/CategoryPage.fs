module ChiGheDocBao.CategoryPage

open System
open System.Text.RegularExpressions
open FSharp.Data 

type private XmlDoc = XmlProvider<"""<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:slash="http://purl.org/rss/1.0/modules/slash/">
  <channel>
    <item>
      <title>Ngộ độc rượu, bệnh nhân thấy ảo giác 'bị ma quỷ truy đuổi'</title>
      <description><![CDATA[<a href="https://vnexpress.net/suc-khoe/ngo-doc-ruou-benh-nhan-thay-ao-giac-bi-ma-quy-truy-duoi-3882905.html"><img width=130 height=100 src="https://i-suckhoe.vnecdn.net/2019/02/18/avatar1550483531917-1550483532-2631-1867-1550496406_180x108.png" ></a></br>Nam bệnh nhân 43 tuổi nhập viện trong tình trạng mơ màng, co giật, nôn mửa, da xanh xao, mê sảng và xuất hiện ảo giác.]]></description>
      <pubDate>Mon, 18 Feb 2019 20:35:51 +0700</pubDate>
      <link>https://vnexpress.net/suc-khoe/ngo-doc-ruou-benh-nhan-thay-ao-giac-bi-ma-quy-truy-duoi-3882905.html</link>
    </item>
    <item>
      <title>Ngôi nhà Nhật chia 13 tầng nhưng không có phòng nào</title>
      <description><![CDATA[<a href="https://vnexpress.net/doi-song/ngoi-nha-nhat-chia-13-tang-nhung-khong-co-phong-nao-3882786.html"><img width=130 height=100 src="https://i-giadinh.vnecdn.net/2019/02/18/05-House-in-Miyamotocho-027-1550479937_180x108.jpg" ></a></br>Không gian các 'tầng' được phân biệt nhờ sự thay đổi của cao độ sàn nhà.]]></description>
      <pubDate>Mon, 18 Feb 2019 20:19:03 +0700</pubDate>
      <link>https://vnexpress.net/doi-song/ngoi-nha-nhat-chia-13-tang-nhung-khong-co-phong-nao-3882786.html</link>
    </item>
  </channel>
</rss>""">

let private parseXml xmlString : Result<XmlDoc.Rss, string> =
    try
        Ok <| XmlDoc.Parse xmlString
    with ex ->
        Error <| "Invalid XML: " + ex.Message

let private parseDescription description =
    let pattern = """src="(.+)" /?></a>(?:</br>)?(.+)"""
    let m = Regex.Match (description, pattern)
    if m.Success then
        let imageUrl = m.Groups.[1].Value
        let content = m.Groups.[2].Value
        Ok (Url imageUrl, content)
    else
        Error <| "Invalid description: " + description

let private parseArticleHead (item : XmlDoc.Item) =
    result {
        let! imageUrl, description = parseDescription item.Description
        return {
            Title = item.Title
            DateTime = item.PubDate.DateTime
            Description = description
            ImageUrl = imageUrl
            Link = Url item.Link
        }
    }

let private parseArticleHeads (rss : XmlDoc.Rss) =
    rss.Channel.Items
    |> Array.map parseArticleHead
    |> Array.choose Result.toOption

let fetchArticleHeads (fetchString : FetchString) categoryUrl =
    asyncResult {
        let! xmlString = fetchString categoryUrl
        let! rss = parseXml xmlString |> AsyncResult.ofResult
        let! ahs = parseArticleHeads rss |> AsyncResult.ofSuccess
        return ahs |> Array.sortByDescending (fun ah -> ah.DateTime)
    }

let fetchThumbnails (fetchImage : FetchImage) articleHeads =
    let fetchImageHardly = fetchImage |> Utils.hard 3
    let stream =
        articleHeads
        |> Stream.create 4 (fun ah -> fetchImageHardly ah.ImageUrl)
    let imageObservable =
        stream.Observable
        |> Observable.choose (fun (index, _, imageResult) ->
            match imageResult with
            | Ok image -> Some (index, image)
            | Error _ -> None
        )
    stream
    |> Stream.changeObservable imageObservable

//===================================================================================================================
//===================================================================================================================

type ArticleHeadViewModel = {
    Title : string
    Description : string
    Image : Image option
}

type View =
    abstract ShowLoading : message:string -> Async<Async<unit>>
    abstract ShowError : title:string -> content:string -> Async<unit>
    abstract Back : unit -> unit
    abstract RefreshAllCells : unit -> unit
    abstract RefreshCell : int -> unit
    abstract NavigateToArticle : ArticleHead -> unit

[<AllowNullLiteral>]
type Presenter (category : Category, fetchArticleHeads, fetchThumbnails, view : View) =
    let mutable articleHeads : ArticleHead [] = Array.empty
    let thumbnails = Collections.Concurrent.ConcurrentDictionary<int, Image> ()
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
        view.NavigateToArticle articleHeads.[index]

    member this.OnBack () =
        stopFetchingThumbnails ()

//===================================================================================================================
//===================================================================================================================
    
open Foundation
open UIKit

[<Register ("ArticleHeadCell")>]
type ArticleHeadCell (handle : IntPtr) =
    inherit ViewCommon.IndexCell (handle)
    [<Outlet>] member val TitleLabel : UILabel = null with get, set
    [<Outlet>] member val DescriptionLabel : UILabel = null with get, set
    [<Outlet>] member val Thumbnail : UIImageView = null with get, set

[<Register ("CategoryViewController")>]
type ViewImpl (handle : IntPtr) =
    inherit UITableViewController (handle)

    let mutable category : Category option = None
    let mutable presenter : Presenter = null

    member this.Inject cat =
        category <- Some cat

    override this.ViewDidLoad () =
        base.ViewDidLoad ()
        presenter <- Presenter (category.Value,
                                fetchArticleHeads Network.fetchString,
                                fetchThumbnails Network.fetchImage_Cached,
                                this)
        this.NavigationItem.Title <- presenter.Title

    override this.RowsInSection (tableView, section) =
        presenter.Length |> nint

    member private this.BuildCell (cell : ArticleHeadCell) =
        let vm = presenter.GetCellViewModel cell.Index

        cell.TitleLabel.Text <- vm.Title
        cell.DescriptionLabel.Text <- vm.Description
        cell.Thumbnail |> ViewCommon.updateImage vm.Image

        cell :> UITableViewCell

    override this.GetCell (tableView, indexPath) =
        let cell = tableView.DequeueReusableCell "ArticleHeadCell" :?> ArticleHeadCell
        cell.Index <- indexPath.Row
        this.BuildCell cell

    override this.RowSelected (tableView, indexPath) =
        presenter.OnCellSelected indexPath.Row

    override this.WillMoveToParentViewController vc =
        base.WillMoveToParentViewController vc
        if not (isNull presenter) then
            presenter.OnBack ()

    interface View with

        member this.ShowLoading message =
            ViewCommon.showToast this message

        member this.ShowError title message =
            ViewCommon.showAlert this title message

        member this.Back () =
            this.InvokeOnMainThread (fun _ ->
                this.NavigationController.PopViewController false |> ignore
            )

        member this.RefreshAllCells () =
            this.InvokeOnMainThread (fun _ ->
                this.TableView.ReloadData ()
            )

        member this.RefreshCell index =
            this.InvokeOnMainThread (fun _ ->
                for cell in this.TableView.VisibleCells do
                    let cell = cell :?> ArticleHeadCell
                    if cell.Index = index then
                        this.BuildCell cell |> ignore
            )

        member this.NavigateToArticle articleHead =
            let vc = this.Storyboard.InstantiateViewController "ArticleViewController"
                        :?> ArticlePage.ViewImpl
            vc.Inject articleHead
            this.NavigationController.PushViewController (vc, false)
