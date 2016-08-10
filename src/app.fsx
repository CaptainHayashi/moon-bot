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

type BBC = XmlProvider<"http://feeds.bbci.co.uk/news/rss.xml">
type Stocks = CsvProvider<"http://ichart.finance.yahoo.com/table.csv?s=MSFT">
type Weather = JsonProvider<"http://api.openweathermap.org/data/2.5/forecast/daily?q=Prague&mode=json&units=metric&cnt=10&APPID=cb63a1cf33894de710a1e3a64f036a27">


// ----------------------------------------------------------------------------
// Web server and query answering
// ----------------------------------------------------------------------------

let answer (question:string) = async {
  let words = question.ToLower().Split(' ') |> List.ofSeq
  match words with
  | ["weather"; city] ->
      let! res = Weather.AsyncLoad(weatherUrl city)
      let items = 
        [ for r in res.List -> 
            sprintf "<li><strong>%s</strong> &mdash; day %i&deg;C / night %i&deg;C</li>" 
              ((toDateTime r.Dt).ToString("D")) 
              (int r.Temp.Day) (int r.Temp.Night) ] 
      let body = items |> String.concat ""
      return "<ul>" + body + "</ul>"

  | ["news"] ->
      let! res = BBC.AsyncGetSample()
      let items = 
        [ for r in res.Channel.Items -> 
            sprintf "<li><a href='%s'>%s</a></li>" r.Link r.Title ] 
      let body = items |> Seq.take 10 |> String.concat ""
      return sprintf "<ul>%s</ul>" body

  | ["stock"; stock] ->
      let! res = Stocks.AsyncLoad(stocksUrl stock)
      let items = 
        [ for r in res.Rows -> 
            sprintf "<li><strong>%s</strong>: %f</li>" 
              (r.Date.ToString("D")) r.Open ] 
      let body = items |> Seq.take 10 |> String.concat ""
      return sprintf "<ul>%s</ul>" body

  | "eval" :: cmds -> 
      return Math.eval (String.concat " " cmds)
  | _ -> 
      return "Sorry Dave, I cannot do that." }

let app =
  choose [
    path "/" >=> Files.browseFile root "index.html"
    path "/query" >=> fun ctx -> async {
        let! res = answer (HttpUtility.UrlDecode(ctx.request.rawQuery))
        return! Successful.OK res ctx }
    Files.browse root ]