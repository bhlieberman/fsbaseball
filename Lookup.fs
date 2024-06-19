namespace Baseball

open FSharp.Data
open System
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Text.RegularExpressions

module Lookup =

    let private registerUrl =
        "https://github.com/chadwickbureau/register/archive/refs/heads/master.zip"

    let private peoplePattern = new Regex("/people.+csv$")

    let cacheDir =
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) + ".fsbaseball"

    type Chadwick = CsvProvider<"~/.fsbaseball/chadwick_register.csv">

    let getCachedFile =
        if
            Directory.Exists("~/.fsbaseball")
            && File.Exists("~/.fsbaseball/chadwick_register.csv")
        then
            Some <| Chadwick.Load("~/.fsbaseball/chadwick_register.csv")
        else
            None

    let unpackZip =
        task {
            let client = new HttpClient()
            let! peopleZip = registerUrl |> client.GetStreamAsync

            let archive = new ZipArchive(peopleZip)

            for entry in archive.Entries do
                if ``not`` <| peoplePattern.Match(entry.FullName).Success then
                    entry |> ignore // need to figure out how to filter these

            archive |> _.ExtractToDirectory(cacheDir)
        }
