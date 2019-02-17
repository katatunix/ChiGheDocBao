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
    inherit UITableViewCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("DescriptionCell")>]
type DescriptionCell (handle : IntPtr) =
    inherit UITableViewCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("ParaCell")>]
type ParaCell (handle : IntPtr) =
    inherit UITableViewCell (handle)
    [<Outlet>] member val Label : UILabel = null with get, set

[<Register ("SecImageCell")>]
type SecImageCell (handle : IntPtr) =
    inherit UITableViewCell (handle)
    [<Outlet>] member val MyImageView : UIImageView = null with get, set
    [<Outlet>] member val CaptionLabel : UILabel = null with get, set

[<Register ("SubtitleCell")>]
type SubtitleCell (handle : IntPtr) =
    inherit UITableViewCell (handle)
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
        presenter <- ArticlePresenter (articleHead, fetchArticle Common.Network.fetchString, this)

    override this.RowsInSection (tableView, section) =
        presenter.GetCellCount () |> nint

    override this.GetCell (tableView, indexPath) =
        let vm = presenter.GetCell indexPath.Row
        match vm with
        | Title str ->
            let cell = tableView.DequeueReusableCell "TitleCell" :?> TitleCell
            cell.Label.Text <- str
            cell :> UITableViewCell
        | Description str ->
            let cell = tableView.DequeueReusableCell "DescriptionCell" :?> DescriptionCell
            cell.Label.Text <- str
            cell :> UITableViewCell
        | Para str ->
            let cell = tableView.DequeueReusableCell "ParaCell" :?> ParaCell
            cell.Label.Text <- str
            cell :> UITableViewCell
        | SecImage str ->
            let cell = tableView.DequeueReusableCell "SecImageCell" :?> SecImageCell
            cell.CaptionLabel.Text <- str
            cell :> UITableViewCell
        | Subtitle str ->
            let cell = tableView.DequeueReusableCell "SubtitleCell" :?> SubtitleCell
            cell.Label.Text <- str
            cell :> UITableViewCell

    interface ArticleView with
        member this.ShowLoading message =
            showToast this message

        member this.ShowError title message =
            showAlert this title message

        member this.Back () =
            this.InvokeOnMainThread (fun _ ->
                this.NavigationController.PopViewController false |> ignore
            )

        member this.OnArticleFetched () =
            this.InvokeOnMainThread (fun _ ->
                this.TableView.ReloadData ()
            )

        member this.SetTitle str =
            this.InvokeOnMainThread (fun _ ->
                this.NavigationItem.Title <- str
            )
