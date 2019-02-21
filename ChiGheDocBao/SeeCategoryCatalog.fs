namespace ChiGheDocBao.SeeCategoryCatalog

open ChiGheDocBao

module Domain =

    open Common.Domain

    let private cat n u = { Name = n; Url = Url u }

    let private vnexpress = [|
        cat "Tin mới nhất" "https://vnexpress.net/rss/tin-moi-nhat.rss"
        cat "Thời sự"      "https://vnexpress.net/rss/thoi-su.rss"
        cat "Pháp luật"    "https://vnexpress.net/rss/phap-luat.rss"
        cat "Thể thao"     "https://vnexpress.net/rss/the-thao.rss"
        cat "Thế giới"     "https://vnexpress.net/rss/the-gioi.rss"
        cat "Giáo dục"     "https://vnexpress.net/rss/giao-duc.rss"
        cat "Kinh doanh"   "https://vnexpress.net/rss/kinh-doanh.rss"
        cat "Giải trí"     "https://vnexpress.net/rss/giai-tri.rss"
        cat "Khởi nghiệp"  "https://vnexpress.net/rss/startup.rss"
        cat "Sức khoẻ"     "https://vnexpress.net/rss/suc-khoe.rss"
        cat "Đời sống"     "https://vnexpress.net/rss/gia-dinh.rss"
        cat "Du lịch"      "https://vnexpress.net/rss/du-lich.rss"
        cat "Khoa học"     "https://vnexpress.net/rss/khoa-hoc.rss"
        cat "Số hoá"       "https://vnexpress.net/rss/so-hoa.rss"
        cat "Ô tô xe máy"  "https://vnexpress.net/rss/oto-xe-may.rss"
        cat "Ý kiến"       "https://vnexpress.net/rss/y-kien.rss"
        cat "Tâm sự"       "https://vnexpress.net/rss/tam-su.rss"
        cat "Cười"         "https://vnexpress.net/rss/cuoi.rss"
    |]
    
    let categories = vnexpress

module Presenter =

    open Common.Domain
    open Domain

    type CategoryCatalogView =
        abstract member ShowCategoryContent : Category -> unit

    [<AllowNullLiteral>]
    type CategoryCatalogPresenter (view : CategoryCatalogView) =
        member this.Count = categories.Length

        member this.GetCategoryName index = categories.[index].Name

        member this.OnCategorySelected index =
            view.ShowCategoryContent categories.[index]

module tvOS =

    open System
    open Foundation
    open UIKit
    open Presenter

    [<Register ("CategoryCatalogView")>]
    type tvOSCategoryCatalogView (handle : IntPtr) =
        inherit UITableViewController (handle)

        let mutable presenter : CategoryCatalogPresenter = null

        override this.ViewDidLoad () =
            base.ViewDidLoad ()
            presenter <- CategoryCatalogPresenter (this)

        override this.RowsInSection (tableView, section) =
            presenter.Count |> nint

        override this.GetCell (tableView, indexPath) =
            let cell = tableView.DequeueReusableCell "CategoryCell"
            cell.TextLabel.Text <- presenter.GetCategoryName indexPath.Row
            cell

        override this.RowSelected (tableView, indexPath) =
            presenter.OnCategorySelected indexPath.Row

        interface CategoryCatalogView with

            member this.ShowCategoryContent category =
                let vc = this.Storyboard.InstantiateViewController "CategoryContentView"
                            :?> SeeCategoryContent.tvOS.tvOSCategoryContentView
                vc.Inject category
                this.NavigationController.PushViewController (vc, false)
