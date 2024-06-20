namespace Baseball

open FSharp.Data
open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Text.RegularExpressions

module Lookup =

    let private registerUrl =
        "https://github.com/chadwickbureau/register/archive/refs/heads/master.zip"

    let private peoplePattern = new Regex("/people.+csv$")


    let cacheDir =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        + "/.fsbaseball"

    type Chadwick = CsvProvider<Sample="~/.fsbaseball/register-master/data/people-0.csv">

    let mutable csvRows = new List<Chadwick.Row>()

    let getCachedFile =
        if Directory.Exists(cacheDir) && File.Exists(cacheDir + "/chadwick_register.csv") then
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

            let archive = new ZipArchive(peopleZip) in

            for entry in archive.Entries do
                if peoplePattern.Match(entry.FullName).Success then
                    Path.Join([| cacheDir; entry.FullName |]) |> entry.ExtractToFile

            let peopleFiles = new DirectoryInfo(dataDir) |> _.EnumerateFiles("*.csv")

            let! csvFiles =
                peopleFiles
                |> Seq.map (fun (f: FileInfo) -> f.FullName |> Chadwick.AsyncLoad)
                |> Async.Parallel
                |> Async.StartAsTask

            return
                csvFiles
                |> Array.map (fun csv -> csv.Rows)
                |> Seq.concat
                |> Seq.iter (fun row -> csvRows.Add row)
                // well this sort of works? need to add the actual Row data
        }
