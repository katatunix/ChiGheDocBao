module ChiGheDocBao.ViewCategory.Domain

open ChiGheDocBao
open Common.Domain

type FetchArticleHeads = Url -> AsyncResult<ArticleHead [], string>

type FetchThumbnails = ArticleHead [] -> Stream<int * Image>

// Impl

open FSharp.Data
open System.Text.RegularExpressions

type private XmlDoc = XmlProvider<"""<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:slash="http://purl.org/rss/1.0/modules/slash/">
  <channel>
    <item>
      <title>Vietnam designer’s collection at New York Fashion Week garners global attention</title>
      <description><![CDATA[<a href="https://e.vnexpress.net/news/life/style/vietnam-designer-s-collection-at-new-york-fashion-week-garners-global-attention-3880956.html"><img width=130 height=100 src="https://i-english.vnecdn.net/2019/02/14/1-1550110709-2805-1550110764_180x108.jpg" ></a></br>Cong Tri’s 2019 fall-winter collection he unveiled at the New York Fashion Week Monday has come in for much praise.]]></description>
      <pubDate>Thu, 14 Feb 2019 13:34:15 +0700</pubDate>
      <link>https://e.vnexpress.net/news/life/style/vietnam-designer-s-collection-at-new-york-fashion-week-garners-global-attention-3880956.html</link>
      <guid>https://e.vnexpress.net/news/life/style/vietnam-designer-s-collection-at-new-york-fashion-week-garners-global-attention-3880956.html</guid>
    </item>
    <item>
      <title>Siêu trăng lớn nhất năm sắp thắp sáng bầu trời tháng 2</title>
      <description><![CDATA[<a href="https://vnexpress.net/khoa-hoc/sieu-trang-lon-nhat-nam-sap-thap-sang-bau-troi-thang-2-3880870.html"><img width=130 height=100 src="https://i-vnexpress.vnecdn.net/2019/02/14/CaptureJPG-1550119982-5959-1550120079_180x108.jpg" ></a></br>Siêu trăng lớn nhất năm 2019 còn gọi là "Trăng tuyết" sẽ xuất hiện trên bầu trời hôm 19/2 khi Mặt Trăng ở vị trí gần Trái Đất nhất.]]></description>
      <pubDate>Thu, 14 Feb 2019 13:30:00 +0700</pubDate>
      <link>https://vnexpress.net/khoa-hoc/sieu-trang-lon-nhat-nam-sap-thap-sang-bau-troi-thang-2-3880870.html</link>
      <guid>https://vnexpress.net/khoa-hoc/sieu-trang-lon-nhat-nam-sap-thap-sang-bau-troi-thang-2-3880870.html</guid>
    </item>
  </channel>
</rss>""">

let private parseXml xmlString : Result<XmlDoc.Rss, string> =
    try
        Ok <| XmlDoc.Parse xmlString
    with ex ->
        Error <| "Invalid XML: " + ex.Message

let private parseDescription description =
    let pattern = """src="(.+)" /?></a>(?:</br>)?(.+)"""
    let m = Regex.Match (description, pattern)
    if m.Success then
        let imageUrl = m.Groups.[1].Value
        let content = m.Groups.[2].Value
        Ok (Url imageUrl, content)
    else
        Error <| "Invalid description: " + description

let private parseArticleHead (item : XmlDoc.Item) =
    result {
        let! imageUrl, description = parseDescription item.Description
        return {
            Title = item.Title
            DateTime = item.PubDate.DateTime
            Description = description
            ImageUrl = imageUrl
            Link = Url item.Link
        }
    }

let private parseArticleHeads (rss : XmlDoc.Rss) =
    rss.Channel.Items
    |> Array.map parseArticleHead
    |> Array.choose Result.toOption

let fetchArticleHeads (fetchString : FetchString) : FetchArticleHeads =
    fun categoryUrl ->
        asyncResult {
            let! xmlString = fetchString categoryUrl
            let! rss = parseXml xmlString |> AsyncResult.ofResult
            return! parseArticleHeads rss |> AsyncResult.ofSuccess
        }

let fetchThumbnails (fetchImage : FetchImage) : FetchThumbnails =
    fun articleHeads ->
        let hardFetchImage = fetchImage |> Utils.hard 3
        let stream =
            articleHeads
            |> Stream.create 4 (fun ah -> hardFetchImage ah.ImageUrl)
        let imageObservable =
            stream.Observable
            |> Observable.choose (fun (index, _, imageResult) ->
                match imageResult with
                | Ok image -> Some (index, image)
                | Error _ -> None
            )
        stream
        |> Stream.changeObservable imageObservable
