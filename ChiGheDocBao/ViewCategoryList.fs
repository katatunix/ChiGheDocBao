namespace ChiGheDocBao.ViewCategoryList
open ChiGheDocBao

module Domain =

    open Common.Domain

    let private cat n u = { Name = n; Url = Url u }

    let vnexpress = [|
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

    let docbao = [|
        cat "Tin mới nhất" "http://docbao.vn/rss/export/home.rss"
        cat "Xã hội" "http://docbao.vn/rss/export/xa-hoi.rss"
        cat "Thế giới" "http://docbao.vn/rss/export/the-gioi.rss"
        cat "Pháp luật" "http://docbao.vn/rss/export/phap-luat.rss"
        cat "Kinh tế" "http://docbao.vn/rss/export/kinh-te.rss"
        cat "Sao 360" "http://docbao.vn/rss/export/sao-360.rss"
        cat "Giải trí" "http://docbao.vn/rss/export/giai-tri.rss"
        cat "Gia đình" "http://docbao.vn/rss/export/gia-dinh.rss"
        cat "Thể thao" "http://docbao.vn/rss/export/the-thao.rss"
        cat "Video" "http://docbao.vn/rss/export/video.rss"
        cat "Giới trẻ" "http://docbao.vn/rss/export/gioi-tre.rss"
        cat "Sức khoẻ " "http://docbao.vn/rss/export/suc-khoe.rss"
        cat "Quân sự" "http://docbao.vn/rss/export/quan-su.rss"
        cat "Ô tô xe máy" "http://docbao.vn/rss/export/o-to-xe-may.rss"
        cat "Đời sống" "http://docbao.vn/rss/export/doi-song.rss"
        cat "Công nghệ" "http://docbao.vn/rss/export/hi-tech.rss"
        cat "Doanh nghiệp" "http://docbao.vn/rss/export/doanh-nghiep.rss"
    |]
    
    let categories = vnexpress

module Presenter =

    open Common.Domain
    open Domain

    type CategoryListView =
        abstract member PushCategoryView : Category -> unit

    [<AllowNullLiteral>]
    type CategoryListPresenter (view : CategoryListView) =
        member this.Count = categories.Length

        member this.GetCategoryName index = categories.[index].Name

        member this.OnCategorySelected index =
            view.PushCategoryView categories.[index]

module tvOS =

    open System
    open Foundation
    open UIKit
    open Presenter
    open ViewCategory.tvOS

    [<Register ("CategoryListView")>]
    type tvOSCategoryListView (handle : IntPtr) =
        inherit UITableViewController (handle)

        let mutable presenter : CategoryListPresenter = null

        override this.ViewDidLoad () =
            base.ViewDidLoad ()
            presenter <- CategoryListPresenter (this)

        override this.RowsInSection (tableView, section) =
            presenter.Count |> nint

        override this.GetCell (tableView, indexPath) =
            let cell = tableView.DequeueReusableCell "CategoryCell"
            cell.TextLabel.Text <- presenter.GetCategoryName indexPath.Row
            cell

        override this.RowSelected (tableView, indexPath) =
            presenter.OnCategorySelected indexPath.Row

        interface CategoryListView with
            member this.PushCategoryView category =
                let vc = this.Storyboard.InstantiateViewController "CategoryView" :?> tvOSCategoryView
                vc.Init category
                this.NavigationController.PushViewController (vc, false)
