module ChiGheDocBao.ViewArticle.tvOS

open System
open Foundation
open UIKit
open ChiGheDocBao
open Common.Domain
open Domain
open Presenter
open Common.tvOS

[<Register ("TitleCell")>]
type TitleCell (handle : IntPtr) =
    inherit IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("DescriptionCell")>]
type DescriptionCell (handle : IntPtr) =
    inherit IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("ParaCell")>]
type ParaCell (handle : IntPtr) =
    inherit IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("SecImageCell")>]
type SecImageCell (handle : IntPtr) =
    inherit IndexCell (handle)
    [<Outlet>] member val MyImageView : UIImageView = null with get, set
    [<Outlet>] member val CaptionLabel : UILabel = null with get, set

[<Register ("SubtitleCell")>]
type SubtitleCell (handle : IntPtr) =
    inherit IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("CaptionCell")>]
type CaptionCell (handle : IntPtr) =
    inherit IndexCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("ArticleView")>]
type tvOSArticleView (handle : IntPtr) =
    inherit UITableViewController (handle)

    let mutable articleHead = ArticleHead.Dummy
    let mutable presenter : ArticlePresenter = null

    member this.Init ah =
        articleHead <- ah

    override this.ViewDidLoad () =
        base.ViewDidLoad ()
        presenter <- ArticlePresenter (articleHead,
                                       fetchArticle Common.Network.fetchString,
                                       fetchSecImages cachedFetchImage,
                                       this)
        this.NavigationItem.Title <- presenter.Title

    override this.RowsInSection (tableView, section) =
        presenter.CellsCount |> nint

    member private this.BuildCell (cell : IndexCell) =
        let vm = presenter.GetCellViewModel cell.Index
        match vm with
        | Title str ->
            (cell :?> TitleCell).Label.Text <- str
        | Description str ->
            (cell :?> DescriptionCell).Label.Text <- str
        | Para str ->
            (cell :?> ParaCell).Label.Text <- str
        | SecImage imageOpt ->
            (cell :?> SecImageCell).MyImageView |> updateImage imageOpt
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
        let cell = tableView.DequeueReusableCell identifier :?> IndexCell
        cell.Index <- indexPath.Row
        this.BuildCell cell

    override this.WillMoveToParentViewController vc =
        base.WillMoveToParentViewController vc
        if not (isNull presenter) then
            presenter.OnBack ()

    interface ArticleView with
        member this.ShowLoading message =
            showToast this message

        member this.ShowError title message =
            showAlert this title message

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
                    let cell = cell :?> IndexCell
                    if cell.Index = index then
                        this.BuildCell cell |> ignore
            )
