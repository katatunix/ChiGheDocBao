module ChiGheDocBao.ArticlePage

open System
open FSharp.Data

type ParaString = private ParaString of string with
    member this.Value = let (ParaString str) = this in str
    static member Create (str : string) =
        str.Split ([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun s ->
            let s = s.Trim ()
            if s.Length > 0 then Some (ParaString s) else None
        )

type Section =
    | Para of ParaString
    | Subtitle of ParaString
    | SecImage of Url
    | Caption of ParaString

type ArticleBody = {
    Sections : Section []
}

type FetchArticleBody = Url -> AsyncResult<ArticleBody, string>

type FetchSecImages = Section [] -> Stream<int * Image>

let private parseHtmlDoc (html : string) =
    try
        Ok <| HtmlDocument.Parse html
    with ex ->
        Error <| "Could not parse html: " + ex.Message

let private findArticleNode (doc : HtmlDocument) =
    ["article.content_detail"; "div.fck_detail"]
    |> List.tryPick (doc.Html().CssSelect >> List.tryHead)

let private (|NormalArticle|SlideshowArticle|) (articleNode : HtmlNode) =
    let nodes = articleNode.CssSelect "div#article_content"
    if nodes.IsEmpty then
        NormalArticle articleNode
    else
        SlideshowArticle nodes.[0]

let private attr name (node : HtmlNode) = node.AttributeValue name

let private parseImageUrl (imgNode : HtmlNode) =
    ["src"; "data-original"]
    |> List.tryPick (fun name ->
        let value = imgNode |> attr name
        if value.StartsWith "http" then Some (Url value) else None
    )

let private parseNormalArticle (articleNode : HtmlNode) = [|
    for node in articleNode.Elements () do

        if node.Name() = "p" && node.HasClass "subtitle" then
            yield! node.InnerText() |> ParaString.Create |> Array.map Subtitle

        elif node.Name() = "p" then
            yield! node.InnerText() |> ParaString.Create |> Array.map Para

        elif node.Name() = "table" && node.HasClass "tplCaption" then
            let imgs = node.CssSelect "img"
            if not imgs.IsEmpty then
                match parseImageUrl imgs.[0] with
                | None -> ()
                | Some url ->
                    yield SecImage url
                    match node.CssSelect "p.Image" |> List.tryHead with
                    | Some node -> yield! node.InnerText() |> ParaString.Create |> Array.map Caption
                    | None -> ()

        elif node.Name() = "div" && node.HasClass "box_tableinsert" then
            for p in node.CssSelect "p" do
                yield! p.InnerText() |> ParaString.Create |> Array.map Para
|]

let private parseSlideshowArticle (articleNode : HtmlNode) = [|
    for node in articleNode.Elements () do

        if node.Name() = "div" && node.HasClass "item_slide_show" then
            let styles = [ "block_thumb_slide_show"; "block_thumb_slide_show_image" ]
            let imgUrl =
                styles
                |> Seq.tryPick (fun style -> node.CssSelect (sprintf ".%s > img" style) |> List.tryHead)
                |> Option.bind parseImageUrl
            match imgUrl with
            | Some url -> yield SecImage url
            | None -> ()

            match node.CssSelect ".desc_cation" |> List.tryHead with
            | Some node ->
                yield! node.InnerText() |> ParaString.Create |> Array.map Caption
            | None -> ()

        elif node.Name() = "div" && node.HasClass "fck_detail" then
            yield! node.InnerText() |> ParaString.Create |> Array.map Para
|]

let private parseSections article =
    match article with
    | NormalArticle node -> parseNormalArticle node
    | SlideshowArticle node -> parseSlideshowArticle node

let fetchArticleBody (fetchString : FetchString) : FetchArticleBody =
    fun articleUrl -> asyncResult {
        let! htmlString = fetchString articleUrl
        let! doc = parseHtmlDoc htmlString |> AsyncResult.ofResult

        let! article =
            findArticleNode doc
            |> Result.ofOption "Chưa hỗ trợ đọc bài viết dạng này"
            |> AsyncResult.ofResult
            
        return { Sections = parseSections article }
    }

let fetchSecImages (fetchImage : FetchImage) : FetchSecImages =
    fun sections ->
        let fetchImageHardly = fetchImage |> Utils.hard 3

        let imageUrls =
            sections
            |> Seq.mapi (fun sectionIndex section -> sectionIndex, section)
            |> Seq.choose (fun (sectionIndex, section) ->
                match section with
                | SecImage url -> Some (sectionIndex, url)
                | _ -> None
            )

        let stream =
            imageUrls
            |> Stream.create 4 (fun (_, imageUrl) -> fetchImageHardly imageUrl)

        let imageObservable =
            stream.Observable
            |> Observable.choose (fun (_, (sectionIndex, _), imageResult) ->
                match imageResult with
                | Ok image -> Some (sectionIndex, image)
                | Error _ -> None
            )

        stream
        |> Stream.changeObservable imageObservable


//======================================================================================================
//======================================================================================================

type View =
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
type Presenter (articleHead : ArticleHead,
                fetchArticleBody : FetchArticleBody,
                fetchSecImages : FetchSecImages,
                view : View) =
    let mutable articleBody : ArticleBody option = None
    let secImages = Collections.Concurrent.ConcurrentDictionary<int, Image> ()
    let mutable stopFetchingSecImages = id

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
        
//======================================================================================================
//======================================================================================================
open Foundation
open UIKit

[<Register ("TitleCell")>]
type TitleCell (handle : IntPtr) =
    inherit ViewCommon.IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("DescriptionCell")>]
type DescriptionCell (handle : IntPtr) =
    inherit ViewCommon.IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("ParaCell")>]
type ParaCell (handle : IntPtr) =
    inherit ViewCommon.IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("SecImageCell")>]
type SecImageCell (handle : IntPtr) =
    inherit ViewCommon.IndexCell (handle)
    [<Outlet>] member val MyImageView : UIImageView = null with get, set
    [<Outlet>] member val CaptionLabel : UILabel = null with get, set

[<Register ("SubtitleCell")>]
type SubtitleCell (handle : IntPtr) =
    inherit ViewCommon.IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("CaptionCell")>]
type CaptionCell (handle : IntPtr) =
    inherit ViewCommon.IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("ArticleViewController")>]
type ViewImpl (handle : IntPtr) =
    inherit UITableViewController (handle)

    let mutable articleHead : ArticleHead option = None
    let mutable presenter : Presenter = null

    member this.Inject ah =
        articleHead <- Some ah

    override this.ViewDidLoad () =
        base.ViewDidLoad ()
        presenter <- Presenter (articleHead.Value,
                                       fetchArticleBody Network.fetchString,
                                       fetchSecImages Network.fetchImage_Cached,
                                       this)
        this.NavigationItem.Title <- presenter.Title

    override this.RowsInSection (tableView, section) =
        presenter.Length |> nint

    member private this.BuildCell (cell : ViewCommon.IndexCell) =
        let vm = presenter.GetCellViewModel cell.Index
        match vm with
        | Title str ->
            (cell :?> TitleCell).Label.Text <- str
        | Description str ->
            (cell :?> DescriptionCell).Label.Text <- str
        | Para str ->
            (cell :?> ParaCell).Label.Text <- str
        | SecImage imageOpt ->
            (cell :?> SecImageCell).MyImageView |> ViewCommon.updateImage imageOpt
        | Subtitle str ->
            (cell :?> SubtitleCell).Label.Text <- str
        | Caption str ->
            (cell :?> CaptionCell).Label.Text <- str
        cell :> UITableViewCell

    override this.GetCell (tableView, indexPath) =
        let vm = presenter.GetCellViewModel indexPath.Row
        let identifier =
            match vm with
            | Title         _ -> "TitleCell"
            | Description   _ -> "DescriptionCell"
            | Para          _ -> "ParaCell"
            | SecImage      _ -> "SecImageCell"
            | Subtitle      _ -> "SubtitleCell"
            | Caption       _ -> "CaptionCell"
        let cell = tableView.DequeueReusableCell identifier :?> ViewCommon.IndexCell
        cell.Index <- indexPath.Row
        this.BuildCell cell

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
                    let cell = cell :?> ViewCommon.IndexCell
                    if cell.Index = index then
                        this.BuildCell cell |> ignore
            )
