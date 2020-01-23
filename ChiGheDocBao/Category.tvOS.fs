module ChiGheDocBao.Category.tvOS

open System
open Foundation
open UIKit
open ChiGheDocBao
open Presenter

[<Register ("ArticleHeadCell")>]
type ArticleHeadCell (handle : IntPtr) =
    inherit Common.tvOS.IndexCell (handle)
    [<Outlet>] member val TitleLabel : UILabel = null with get, set
    [<Outlet>] member val DescriptionLabel : UILabel = null with get, set
    [<Outlet>] member val Thumbnail : UIImageView = null with get, set

[<Register ("CategoryView")>]
type tvOSCategoryView (handle : IntPtr) =
    inherit UITableViewController (handle)

    let mutable category : Common.Domain.Category option = None
    let mutable presenter : CategoryPresenter = null

    member this.Inject cat =
        category <- Some cat

    override this.ViewDidLoad () =
        base.ViewDidLoad ()
        presenter <- CategoryPresenter (category.Value,
                                               Domain.fetchArticleHeads Common.Network.fetchString,
                                               Domain.fetchThumbnails Common.tvOS.cachedFetchImage,
                                               this)
        this.NavigationItem.Title <- presenter.Title

    override this.RowsInSection (tableView, section) =
        presenter.Length |> nint

    member private this.BuildCell (cell : ArticleHeadCell) =
        let vm = presenter.GetCellViewModel cell.Index

        cell.TitleLabel.Text <- vm.Title
        cell.DescriptionLabel.Text <- vm.Description
        cell.Thumbnail |> Common.tvOS.updateImage vm.Image

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

    interface CategoryView with

        member this.ShowLoading message =
            Common.tvOS.showToast this message

        member this.ShowError title message =
            Common.tvOS.showAlert this title message

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
            let vc = this.Storyboard.InstantiateViewController "ArticleContentView"
                        :?> Article.tvOS.tvOSArticleView
            vc.Inject articleHead
            this.NavigationController.PushViewController (vc, false)
