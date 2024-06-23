module baseball.LookupTest

open Baseball.Lookup
open NUnit.Framework
open System
open System.IO

[<TestFixture>]
type LookupTest() =
    member private _.userProfile =
        Environment.GetFolderPath Environment.SpecialFolder.UserProfile

    member private this.peopleDir =
        this.userProfile + "/.fsbaseball" + "/register-master/data"

    [<Test>]
    member this.cacheDirIsValid() =
        Assert.That(cacheDir, Is.EqualTo(this.userProfile + "/.fsbaseball"))

    [<SetUp>]
    member this.SetUp() =
        [| this.userProfile; "/.fsbaseball"; "/register-master/data" |]
        |> Path.Join
        |> Directory.CreateDirectory
        |> ignore

    [<TearDown>]
    member _.TearDown() =
        let cache = new DirectoryInfo(cacheDir)

        if cache.Exists then
            cache.Delete(true)

    [<Test>]
    member this.checkUnpackZip() =
        Assert.DoesNotThrowAsync(fun _ -> unpackZip)
        this.checkPeopleTablesExists()

    member private this.checkPeopleTablesExists() =
        let cache = new DirectoryInfo(this.peopleDir)

        cache.EnumerateFiles("*.csv")
        |> Seq.length
        |> fun l -> Assert.That(l, Is.GreaterThan 0)
