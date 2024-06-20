namespace Baseball

open FSharp.Data
open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Text.RegularExpressions
open System.Threading

module Lookup =

    let private registerUrl =
        "https://github.com/chadwickbureau/register/archive/refs/heads/master.zip"

    let private peoplePattern = new Regex("/people.+csv$")


    let cacheDir =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        + "/.fsbaseball"

    [<Literal>]
    let sampleFile = __SOURCE_DIRECTORY__ + "/chadwick_sample.csv"

    type Chadwick = CsvProvider<Sample=sampleFile, AssumeMissingValues=true>

    let mutable csvRows = new List<(string * string * obj)>()

    let getCachedFile =
        if File.Exists(cacheDir + "/chadwick_register.csv") then
            Some <| Chadwick.Load(cacheDir + "/chadwick_register.csv")
        else
            None

    let unpackZip =

        task {
            let client = new HttpClient()
            let! peopleZip = registerUrl |> client.GetStreamAsync

            let dataDir = cacheDir + "/register-master/data"

            if ``not`` <| Path.Exists(dataDir) then
                Directory.CreateDirectory(dataDir) |> ignore

            use archive = new ZipArchive(peopleZip) in

            for entry in archive.Entries do
                if peoplePattern.Match(entry.FullName).Success then
                    Path.Join([| cacheDir; entry.FullName |]) |> entry.ExtractToFile

            let peopleFiles = new DirectoryInfo(dataDir) |> _.EnumerateFiles("*.csv")

            return!
                peopleFiles
                |> Seq.map (fun (f: FileInfo) -> f.FullName |> Chadwick.AsyncLoad)
                |> Async.Parallel
                |> Async.StartAsTask
        }

    let private cancellationToken = new CancellationTokenSource(10000)

    let createRegister =
        task {
            let! unpacked = unpackZip

            unpacked
            |> Seq.map _.Rows
            |> Seq.concat
            |> Seq.iter (fun (row: Chadwick.Row) -> csvRows.Add <| (row.Name_first, row.Name_last, row.Key_mlbam))
        }
        |> _.Wait(cancellationToken.Token)

    let runCreate =
        try
            createRegister
        with
        | :? AggregateException as ex ->
            for exc in ex.InnerExceptions do
                Console.WriteLine exc

    // TODO: write the rows to a file!