namespace Baseball

open FSharp.Data
open FuzzySharp
open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Text.RegularExpressions

module Lookup =

    let private registerUrl =
        "https://github.com/chadwickbureau/register/archive/refs/heads/master.zip"

    let private peoplePattern = new Regex "/people.+csv$"


    let cacheDir =
        Environment.GetFolderPath Environment.SpecialFolder.UserProfile + "/.fsbaseball"

    [<Literal>]
    let sampleFile = __SOURCE_DIRECTORY__ + "/chadwick_sample.csv"

    type Chadwick = CsvProvider<Sample=sampleFile, AssumeMissingValues=true>

    type MlbLookup =
        CsvProvider<
            "Ben,Lieberman,12345,",
            Schema="Name_first (string),Name_last (string),Key_mlbam (Nullable<int>), Whole_name (string option)",
            HasHeaders=false,
            AssumeMissingValues=true
         >

    let getCachedFile = Path.Join [| cacheDir; "/chadwick_register.csv" |]

    let unpackZip =

        task {
            let client = new HttpClient()

            let dataDir = cacheDir + "/register-master/data"

            if not (Path.Exists dataDir) then
                Directory.CreateDirectory dataDir |> ignore

            use! peopleZip = registerUrl |> client.GetStreamAsync
            use archive = new ZipArchive(peopleZip)

            for entry in archive.Entries do
                if peoplePattern.Match(entry.FullName).Success then
                    Path.Join [| cacheDir; entry.FullName |] |> entry.ExtractToFile

            let peopleFiles = new DirectoryInfo(dataDir) |> _.EnumerateFiles("*.csv")

            return!
                peopleFiles
                |> Seq.map (_.FullName >> Chadwick.AsyncLoad)
                |> Async.Parallel
                |> Async.StartAsTask
        }



    let createRegister () =
        task {

            let! unpacked = unpackZip

            let lookupTable =
                unpacked
                |> Seq.map _.Rows
                |> Seq.concat
                |> Seq.map (fun row -> new MlbLookup.Row(row.Name_first, row.Name_last, row.Key_mlbam, None))

            let registerPath = cacheDir + "/chadwick_register.csv"

            if not (File.Exists registerPath) then
                File.Create(registerPath) |> ignore

            return!
                async {
                    File.WriteAllLines(registerPath, [| "Name_first,Name_last,Key_mlbam" |])
                    new MlbLookup(lookupTable) |> _.Save(registerPath)
                }
                |> Async.StartAsTask

        }

    let getRegister =

        if Path.Exists getCachedFile then
            Console.WriteLine "Retrieving the cached dataset."
            let location = getCachedFile in
            MlbLookup.AsyncLoad location
        else if not (Path.Exists getCachedFile) then
            try
                if createRegister () |> _.Wait(10000) then
                    let location = getCachedFile in MlbLookup.AsyncLoad location
                else
                    failwith "Could not load the Chadwick database."
            with :? Exception ->
                failwith "Something went wrong retrieving the Chadwick database."
        else
            failwith "Could not load or retrieve dataset."

    type PlayerProfile =
        { name: string
          mlbId: Nullable<int> }

        static member create (lookup: MlbLookup) (name: string) =

            lookup.Rows
            |> Seq.collect (fun (r: MlbLookup.Row) ->
                let score = Fuzz.Ratio(name, String.Join(" ", [| r.Name_first; r.Name_last |]))

                Seq.singleton
                    {| score = score
                       name = name
                       id = r.``Key_mlbam (Nullable<int>)`` |})
            |> Seq.sortByDescending _.score
            |> Seq.take 5
            |> Some

    let findPlayer (lookup_table: MlbLookup) (player_first: string option) (player_last: string option) =
        let provideEmpty = Option.defaultValue String.Empty >> Some

        (provideEmpty player_first, provideEmpty player_last)
        ||> Option.map2 (fun fst lst -> String.Join(" ", [| fst; lst |]))
        |> Option.bind (fun name -> PlayerProfile.create lookup_table name)

    let search = getRegister |> Async.RunSynchronously |> findPlayer
