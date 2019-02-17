module ChiGheDocBao.ViewCategory.tvOS

open System
open Foundation
open UIKit
open ChiGheDocBao
open Common.Domain
open Domain
open Presenter
open Common.tvOS
open ViewArticle.tvOS

let private cachedDownloadImage = Common.Network.fetchImage |> Utils.memo

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

[<Register ("CategoryView")>]
type tvOSCategoryView (handle : IntPtr) =
    inherit UITableViewController (handle)

    let mutable category = Category.Dummy
    let mutable presenter : CategoryPresenter = null

    member this.Init cat =
        category <- cat

    override this.ViewDidLoad () =
        base.ViewDidLoad ()

        this.NavigationItem.Title <- category.Name
        presenter <- CategoryPresenter (category.Url,
                                        fetchArticleHeads Common.Network.fetchString,
                                        fetchThumbnails cachedDownloadImage,
                                        this)

    override this.RowsInSection (tableView, section) =
        presenter.GetArticleHeadsCount () |> nint

    member private this.UpdateCell (cell : ArticleHeadCell) =
        let vm = presenter.GetArticleHead cell.Index

        cell.TitleLabel.Text <- vm.Title
        cell.DescriptionLabel.Text <- vm.Description

        match vm.Image with
        | Some image ->
            cell.Thumbnail.Image <- uiImage image
        | None ->
            cell.Thumbnail.Image <- null

        cell :> UITableViewCell

    override this.GetCell (tableView, indexPath) =
        let cell = tableView.DequeueReusableCell "ArticleHeadCell" :?> ArticleHeadCell
        cell.Index <- indexPath.Row
        this.UpdateCell cell

    override this.RowSelected (tableView, indexPath) =
        presenter.OnArticleHeadSelected indexPath.Row

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

        member this.OnAhsFetched () =
            this.InvokeOnMainThread (fun _ ->
                this.TableView.ReloadData ()
            )

        member this.OnThumbnailFetched index =
            this.InvokeOnMainThread (fun _ ->
                for cell in this.TableView.VisibleCells do
                    let cell = cell :?> ArticleHeadCell
                    if cell.Index = index then
                        this.UpdateCell cell |> ignore
            )

        member this.PushArticleView articleHead =
            let vc = this.Storyboard.InstantiateViewController "ArticleView" :?> tvOSArticleView
            vc.Init articleHead
            this.NavigationController.PushViewController (vc, false)
