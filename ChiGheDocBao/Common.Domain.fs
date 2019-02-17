module ChiGheDocBao.Common.Domain

open System

type Url = Url of string with
    member this.Value = let (Url x) = this in x
    static member Dummy = Url ""

type Image = Image of byte [] with
    member this.Value = let (Image x) = this in x
    static member Dummy = Image Array.empty

type Category = { Name : string; Url : Url } with
    static member Dummy = { Name = ""; Url = Url.Dummy }

type ArticleHead = {
    Title : string
    DateTime : DateTime
    Description : string
    ImageUrl : Url
    Link : Url
} with
    static member Dummy = {
        Title = ""; DateTime = DateTime.Now; Description = ""; ImageUrl = Url.Dummy; Link = Url.Dummy
    }

type FetchString = Url -> AsyncResult<string, string>
type FetchImage = Url -> Result<Image, string>
