#if INTERACTIVE
#r "System.Xml.Linq.dll"
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../packages/Suave/lib/net40/Suave.dll"
#load "math.fs"
#else
module App
#endif
open Suave
open System
open System.IO
open System.Web
open FSharp.Data
open Suave.Filters
open Suave.Writers
open Suave.Operators

// ----------------------------------------------------------------------------
// Various helper functions
// ----------------------------------------------------------------------------

// Find the 'web' directory with static files (like 'index.html'). This is
// a bit ugly, because it works differently locally and on Azure
let root =
  let asm =
    if System.Reflection.Assembly.GetExecutingAssembly().IsDynamic then __SOURCE_DIRECTORY__
    else IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
  IO.Path.GetFullPath(Path.Combine(asm, "..", "web"))

/// Creates a URL for getting wether forecaset from OpenWeatherMap
let weatherUrl (place:string) =
  "http://api.openweathermap.org/data/2.5/forecast/daily?q=" +
    HttpUtility.UrlEncode(place) +
    "&mode=json&units=metric&cnt=10&APPID=cb63a1cf33894de710a1e3a64f036a27"

/// Creates a URL for getting urban dictionary definitions
let urbanUrl (term:string) =
  "http://api.urbandictionary.com/v0/define?term=" +
    HttpUtility.UrlEncode(term)

/// Creates a URL for getting GIFs from Giphy
let giphyUrl (term:string) =
  sprintf "http://api.giphy.com/v1/gifs/search?q=%s&api_key=%s"
    (HttpUtility.UrlEncode(term))
    "dc6zaTOxFJmzC" // beta API key

/// Creates a URL for getting stock prices form Yahooo
let stocksUrl stock =
  "http://ichart.finance.yahoo.com/table.csv?s=" + stock

/// Turn Unix timestamp into .NET DataTime object
let toDateTime (timestamp:int) =
  let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
  start.AddSeconds(float timestamp).ToLocalTime()


// ----------------------------------------------------------------------------
// Type providers for accessing data
// ----------------------------------------------------------------------------

type RSS = XmlProvider<"http://feeds.bbci.co.uk/news/rss.xml">
type Stocks = CsvProvider<"http://ichart.finance.yahoo.com/table.csv?s=MSFT">
type Weather = JsonProvider<"http://api.openweathermap.org/data/2.5/forecast/daily?q=Prague&mode=json&units=metric&cnt=10&APPID=cb63a1cf33894de710a1e3a64f036a27">
type Urban = JsonProvider<"http://api.urbandictionary.com/v0/define?term=hayashi">
type Giphy = JsonProvider<"http://api.giphy.com/v1/gifs/search?q=funny+cat&api_key=dc6zaTOxFJmzC">


// ----------------------------------------------------------------------------
// Web server and query answering
// ----------------------------------------------------------------------------

// TASK #4: For more see 'math.fs'. This implements a simple evaluator
// of binary expressions using parser combinators. The possibilities are
// limitless :-)

// ----------------------------------------------------------------------------
// Producing answers
// ----------------------------------------------------------------------------

let rss provider =
  async {
    let! res = RSS.AsyncLoad provider
    let items =
      [ for r in res.Channel.Items ->
          sprintf "<dt><a href=\"%s\">%s</a></dt><dd>%s</dd>" r.Link r.Title r.Description ]
    let body = items |> Seq.take 10 |> String.concat ""
    return sprintf "<dl>%s</dl>" body
  }

let answer (question:string) =
  async {
    let words = question.ToLower().Split(' ') |> List.ofSeq
    match words with
    | "stock" :: stock :: [] ->
        let! res = Stocks.AsyncLoad(stocksUrl stock)
        let items =
          [ for r in res.Rows ->
              sprintf "<li><strong>%s</strong>: %f</li>"
                (r.Date.ToString("D")) r.Open ]
        let body = items |> Seq.take 10 |> String.concat ""
        return sprintf "<ul>%s</ul>" body
    | "news" :: [] -> return! (rss "https://feeds.bbci.co.uk/news/rss.xml")
    | "yc" :: [] -> return! (rss "https://news.ycombinator.com/rss")
    | "weather" :: place :: [] ->
      let! res = Weather.AsyncLoad(weatherUrl place)
      let items =
        [ for r in res.List ->
            sprintf "<li><strong>%s</strong>: % .2f&deg;C</li>"
              ((toDateTime r.Dt).ToString("D")) r.Temp.Day ]
      let body = items |> Seq.take 10 |> String.concat ""
      return sprintf "<ul>%s</ul>" body
    | "urban" :: xs ->
      let query = String.concat " " xs
      let! res = Urban.AsyncLoad(urbanUrl query)
      let items =
        [ for r in res.List ->
            sprintf "<dt><a href=\"%s\">%s</a> by %s (+%d -%d)</t><dd>%s</dd>"
              r.Permalink r.Word r.Author r.ThumbsUp r.ThumbsDown r.Definition ]
      let body = items |> String.concat ""
      return sprintf "<dl>%s</dl>" body
    | "giphy" :: xs ->
      let query = String.concat " " xs
      let! res = Giphy.AsyncLoad(giphyUrl query)
      let items =
        [ for r in res.Data ->
            sprintf "<li><a href=\"%s\"><img src=\"%s\" alt=\"%s\"/></a></li>"
              r.Url r.Images.FixedHeight.Url r.Id ]
      let body = items |> Seq.truncate 5 |> String.concat ""
      return sprintf "<ul>%s</ul>" body
    | "eval" :: cmds -> return Math.eval (String.concat " " cmds)
    | _ -> return "Sorry Dave, I cannot do that."
  }

let app =
  choose [
    path "/" >=> Files.browseFile root "index.html"
    path "/query" >=> fun ctx -> async {
        let! res = answer (HttpUtility.UrlDecode(ctx.request.rawQuery))
        return! Successful.OK res ctx }
    Files.browse root ]
