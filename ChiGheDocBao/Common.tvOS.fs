module ChiGheDocBao.Common.tvOS

open System
open System.Threading
open UIKit
open Foundation
open ChiGheDocBao
open Common.Domain

let cachedFetchImage : FetchImage = Common.Network.fetchImage |> Utils.memorize

let runOnViewThread (vc : UIViewController) f =
    fun x ->
        async {
            use mre = new ManualResetEvent (false)
            let mutable y = None
            vc.InvokeOnMainThread (fun _ -> y <- f x |> Some; mre.Set () |> ignore)
            let! _ = Async.AwaitWaitHandle mre
            return y.Value
        }

let private reduce x =
    async {
        let! y = x
        return! y
    }

let runOnViewThread2 (vc : UIViewController) f =
    runOnViewThread vc f
    >> reduce

let private awaitThenDispose handle =
    async {
        do! Async.AwaitWaitHandle handle |> Async.Ignore
        handle.Dispose ()
    }

let showAlert (vc : UIViewController) title content =
    ()
    |> runOnViewThread2 vc (fun _ ->
        let mre = new ManualResetEvent (false)
        let alert = UIAlertController.Create (title, content, UIAlertControllerStyle.Alert)
        alert.AddAction <| UIAlertAction.Create ("OK", UIAlertActionStyle.Default, fun _ -> mre.Set () |> ignore)
        vc.PresentViewController (alert, false, null)
        awaitThenDispose mre
    )

let showToast (vc : UIViewController) content =
    ()
    |> runOnViewThread vc (fun _ ->
        let alert = UIAlertController.Create (null, content, UIAlertControllerStyle.Alert)
        vc.PresentViewController (alert, false, null)
        ()
        |> runOnViewThread2 vc (fun _ ->
            let mre = new ManualResetEvent (false)
            alert.DismissViewController (false, Action (mre.Set >> ignore))
            alert.Dispose ()
            awaitThenDispose mre
        )
    )

let updateImage (image : Image option) (view : UIImageView) =
    match image with
    | Some (Image bytes) ->
        view.Image <- bytes |> NSData.FromArray |> UIImage.LoadFromData
    | None ->
        view.Image <- null

type EstimatedTableViewController (handle : IntPtr) =
    inherit UITableViewController (handle)

    let heightCache = Collections.Generic.Dictionary<int, nfloat> ()

    override this.WillDisplay (tv, cell, indexPath) =
        heightCache.[indexPath.Row] <- cell.Frame.Size.Height

    override this.EstimatedHeight (tv, indexPath) =
        match heightCache.TryGetValue indexPath.Row with
        | true, height -> height
        | _ -> UITableView.AutomaticDimension
        
type IndexCell (handle : IntPtr) =
    inherit UITableViewCell (handle)
    member val Index : int = 0 with get, set
