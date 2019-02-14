module ChiGheDocBao.Common.Domain

type Url = Url of string with
    member this.Value = let (Url x) = this in x
    static member Dummy = Url ""

type Image = Image of byte [] with
    member this.Value = let (Image x) = this in x
    static member Dummy = Image Array.empty

type Category = { Name : string; Url : Url } with
    static member Dummy = { Name = ""; Url = Url.Dummy }

type DownloadString = Url -> Result<string, string>
type DownloadImage = Url -> Result<Image, string>
