module ChiGheDocBao.SeeCategoryContent.Domain

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
      <title>Ngộ độc rượu, bệnh nhân thấy ảo giác 'bị ma quỷ truy đuổi'</title>
      <description><![CDATA[<a href="https://vnexpress.net/suc-khoe/ngo-doc-ruou-benh-nhan-thay-ao-giac-bi-ma-quy-truy-duoi-3882905.html"><img width=130 height=100 src="https://i-suckhoe.vnecdn.net/2019/02/18/avatar1550483531917-1550483532-2631-1867-1550496406_180x108.png" ></a></br>Nam bệnh nhân 43 tuổi nhập viện trong tình trạng mơ màng, co giật, nôn mửa, da xanh xao, mê sảng và xuất hiện ảo giác.]]></description>
      <pubDate>Mon, 18 Feb 2019 20:35:51 +0700</pubDate>
      <link>https://vnexpress.net/suc-khoe/ngo-doc-ruou-benh-nhan-thay-ao-giac-bi-ma-quy-truy-duoi-3882905.html</link>
    </item>
    <item>
      <title>Ngôi nhà Nhật chia 13 tầng nhưng không có phòng nào</title>
      <description><![CDATA[<a href="https://vnexpress.net/doi-song/ngoi-nha-nhat-chia-13-tang-nhung-khong-co-phong-nao-3882786.html"><img width=130 height=100 src="https://i-giadinh.vnecdn.net/2019/02/18/05-House-in-Miyamotocho-027-1550479937_180x108.jpg" ></a></br>Không gian các 'tầng' được phân biệt nhờ sự thay đổi của cao độ sàn nhà.]]></description>
      <pubDate>Mon, 18 Feb 2019 20:19:03 +0700</pubDate>
      <link>https://vnexpress.net/doi-song/ngoi-nha-nhat-chia-13-tang-nhung-khong-co-phong-nao-3882786.html</link>
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
            let! ahs = parseArticleHeads rss |> AsyncResult.ofSuccess
            return ahs |> Array.sortByDescending (fun ah -> ah.DateTime)
        }

let fetchThumbnails (fetchImage : FetchImage) : FetchThumbnails =
    fun articleHeads ->
        let fetchImageHardly = fetchImage |> Utils.hard 3
        let stream =
            articleHeads
            |> Stream.create 4 (fun ah -> fetchImageHardly ah.ImageUrl)
        let imageObservable =
            stream.Observable
            |> Observable.choose (fun (index, _, imageResult) ->
                match imageResult with
                | Ok image -> Some (index, image)
                | Error _ -> None
            )
        stream
        |> Stream.changeObservable imageObservable
