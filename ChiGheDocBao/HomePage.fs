module ChiGheDocBao.HomePage

let private cat n u = { Name = n; Url = Url u }

let private categories = [|
    cat "Tin mới nhất"  "https://vnexpress.net/rss/tin-moi-nhat.rss"
    cat "Thời sự"       "https://vnexpress.net/rss/thoi-su.rss"
    cat "Pháp luật"     "https://vnexpress.net/rss/phap-luat.rss"
    cat "Thể thao"      "https://vnexpress.net/rss/the-thao.rss"
    cat "Thế giới"      "https://vnexpress.net/rss/the-gioi.rss"
    cat "Giáo dục"      "https://vnexpress.net/rss/giao-duc.rss"
    cat "Kinh doanh"    "https://vnexpress.net/rss/kinh-doanh.rss"
    cat "Giải trí"      "https://vnexpress.net/rss/giai-tri.rss"
    cat "Khởi nghiệp"   "https://vnexpress.net/rss/startup.rss"
    cat "Sức khoẻ"      "https://vnexpress.net/rss/suc-khoe.rss"
    cat "Đời sống"      "https://vnexpress.net/rss/gia-dinh.rss"
    cat "Du lịch"       "https://vnexpress.net/rss/du-lich.rss"
    cat "Khoa học"      "https://vnexpress.net/rss/khoa-hoc.rss"
    cat "Số hoá"        "https://vnexpress.net/rss/so-hoa.rss"
    cat "Ô tô xe máy"   "https://vnexpress.net/rss/oto-xe-may.rss"
    cat "Ý kiến"        "https://vnexpress.net/rss/y-kien.rss"
    cat "Tâm sự"        "https://vnexpress.net/rss/tam-su.rss"
    cat "Cười"          "https://vnexpress.net/rss/cuoi.rss"
|]

type View =
    abstract member NavigateToCategory : Category -> unit

[<AllowNullLiteral>]
type Presenter (view : View) =
    member this.Count = categories.Length
    member this.GetCategoryName index = categories.[index].Name
    member this.OnCategorySelected index = view.NavigateToCategory categories.[index]

open System
open Foundation
open UIKit

[<Register ("HomeViewController")>]
type ViewImpl (handle : IntPtr) =
    inherit UITableViewController (handle)

    let mutable presenter : Presenter = null

    override this.ViewDidLoad () =
        base.ViewDidLoad ()
        let dict = NSBundle.MainBundle.InfoDictionary
        let get (key : string) = dict.[key].ToString ()
        this.Title <- sprintf "%s v%s (%s)" (get "CFBundleDisplayName") (get "CFBundleShortVersionString") (get "CFBundleVersion")
        presenter <- Presenter (this)

    override this.RowsInSection (tableView, section) =
        presenter.Count |> nint

    override this.GetCell (tableView, indexPath) =
        let cell = tableView.DequeueReusableCell "CategoryCell"
        cell.TextLabel.Text <- presenter.GetCategoryName indexPath.Row
        cell

    override this.RowSelected (tableView, indexPath) =
        presenter.OnCategorySelected indexPath.Row

    interface View with

        member this.NavigateToCategory category =
            let vc = this.Storyboard.InstantiateViewController "CategoryViewController"
                        :?> CategoryPage.ViewImpl
            vc.Inject category
            this.NavigationController.PushViewController (vc, false)
