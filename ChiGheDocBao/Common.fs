[<AutoOpen>]
module ChiGheDocBao.Common

open System

type Url = Url of string with
    member this.Value = let (Url x) = this in x

type Image = Image of byte [] with
    member this.Value = let (Image x) = this in x

type Category = {
    Name : string
    Url : Url
}

type ArticleHead = {
    Title : string
    DateTime : DateTime
    Description : string
    ImageUrl : Url
    Link : Url
}

type FetchString = Url -> AsyncResult<string, string>
type FetchImage = Url -> Result<Image, string>
