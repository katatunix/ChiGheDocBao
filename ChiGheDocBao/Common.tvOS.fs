module ChiGheDocBao.Common.tvOS

open System
open UIKit

let showMessageBox (vc : UIViewController) title content completion =
    let alert = UIAlertController.Create (title, content, UIAlertControllerStyle.Alert)
    alert.AddAction <| UIAlertAction.Create ("OK", UIAlertActionStyle.Default, fun _ -> completion ())
    vc.PresentViewController (alert, false, null)

let showToast (vc : UIViewController) content =
    let alert = UIAlertController.Create (null, content, UIAlertControllerStyle.Alert)
    vc.PresentViewController (alert, false, null)
    fun completion ->
        vc.InvokeOnMainThread (fun _ ->
            alert.DismissViewController (false, Action completion)
            alert.Dispose ())

type EstimatedTableViewController (handle : IntPtr) =
    inherit UITableViewController (handle)

    let heightCache = Collections.Generic.Dictionary<int, nfloat> ()

    override this.WillDisplay (tv, cell, indexPath) =
        heightCache.[indexPath.Row] <- cell.Frame.Size.Height

    override this.EstimatedHeight (tv, indexPath) =
        match heightCache.TryGetValue indexPath.Row with
        | true, height -> height
        | _ -> UITableView.AutomaticDimension
