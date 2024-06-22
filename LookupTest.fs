module baseball.LookupTest

open Baseball.Lookup
open NUnit.Framework
open System
open System.IO

[<TestFixture>]
type LookupTest() =
    [<Test>]
    member _.cacheDirIsValid() =
        let userProfile = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
        Assert.That(cacheDir, Is.EqualTo(userProfile + "/.fsbaseball"))

    [<SetUp>]
    member _.SetUp() =
        let userProfile = Environment.GetFolderPath Environment.SpecialFolder.UserProfile

        [| userProfile; "/.fsbaseball" |]
        |> Path.Join
        |> Directory.CreateDirectory
        |> ignore

    [<TearDown>]
    member _.TearDown() =
        let cache = new DirectoryInfo(cacheDir)

        if cache.Exists then
            cache.Delete(true)

    [<Test>]
    member _.checkUnpackZip() =
        Assert.DoesNotThrowAsync(fun _ -> unpackZip)

    [<Test>]
    member _.checkLookupTableExists() =
        let cache = new DirectoryInfo(cacheDir)

        cache.EnumerateFiles("*.csv")
        |> Seq.tryFind (fun (f: FileInfo) -> f.Name = "chadwick_register.csv")
        |> Option.isSome
        |> Assert.IsTrue
