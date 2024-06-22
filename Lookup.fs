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

    let private peoplePattern = new Regex "/people.+csv$"


    let cacheDir =
        Environment.GetFolderPath Environment.SpecialFolder.UserProfile + "/.fsbaseball"

    [<Literal>]
    let sampleFile = __SOURCE_DIRECTORY__ + "/chadwick_sample.csv"

    type Chadwick = CsvProvider<Sample=sampleFile, AssumeMissingValues=true>

    type MlbLookup =
        CsvProvider<
            "Ben,Lieberman,12345",
            Schema="Name_first (string),Name_last (string),Key_mlbam (Nullable<int>)",
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
                |> Seq.map (fun row -> new MlbLookup.Row(row.Name_first, row.Name_last, row.Key_mlbam))

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
        |> _.Wait(10000)

    let getRegister =

        if Path.Exists getCachedFile then
            Console.WriteLine "Retrieving the cached dataset."
            let location = getCachedFile in
            MlbLookup.Load location
        else if createRegister () then
            Console.WriteLine "retrieving the Chadwick database, please wait..."
            Console.WriteLine "Creating the register file..."
            let location = getCachedFile in
            MlbLookup.Load location
        else
            new MlbLookup([])

    type PlayerProfile =
        { name: string
          mlbId: Nullable<int> }

        static member create (lookup: MlbLookup) (name: string) =
            Seq.tryFind
                (fun (row: MlbLookup.Row) ->
                    let wholeName = row.Name_first + row.Name_last
                    wholeName = name)
                lookup.Rows
            |> Option.bind (fun row ->
                Some
                    { name = row.Name_first + row.Name_last
                      mlbId = row.``Key_mlbam (Nullable<int>)`` })

    let findPlayer (lookup_table: MlbLookup) (player_first: string option) (player_last: string option) =
        let provideEmpty = Option.defaultValue String.Empty >> Some

        (provideEmpty player_first, provideEmpty player_last)
        ||> Option.map2 (fun fst lst -> fst + lst)
        |> Option.bind (fun name -> PlayerProfile.create lookup_table name)

    let search = findPlayer getRegister