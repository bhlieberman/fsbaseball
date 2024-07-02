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

    [<OneTimeSetUp>]
    member this.SetUp() =
        [| this.userProfile; "/.fsbaseball"; "/register-master/data" |]
        |> Path.Join
        |> Directory.CreateDirectory
        |> ignore

    [<OneTimeSetUp>]
    member this.createCsv() =
        let register =
            Path.Join [| this.userProfile; "/.fsbaseball"; "chadwick_register.csv" |]

        File.WriteAllLines(register, [| "Name_first,Name_last,Key_mlbam,Whole_name" |])

    [<OneTimeTearDown>]
    member _.TearDown() =
        let cache = new FileInfo(cacheDir + "chadwick_register.csv")

        if cache.Exists then
            cache.Delete()

    [<Test>]
    member this.checkUnpackZip() =
        Assert.DoesNotThrowAsync(fun _ -> unpackZip)

    [<Test>]
    member _.checkGetRegister() =
        Assert.DoesNotThrowAsync(fun _ -> createRegister ())

    [<Test>]
    [<Ignore("Not useful when the cached dataset is being used.")>]
    member this.checkPeopleTablesExists() =
        let cache = new DirectoryInfo(this.peopleDir)

        cache.EnumerateFiles("*.csv")
        |> Seq.length
        |> fun l -> Assert.That(l, Is.GreaterThan 0)

    [<Test>]
    member _.checkForExistingName() =
        // let ex = Assert.Throws<InvalidOperationException>(fun _ -> search (Some "Adley") (Some "Rutschman") |> ignore)
        // Assert.That(ex.Message, Is.EqualTo("Tried to take past the end of a sequence."))
        let results = search (Some "Adley") (Some "Rutschman") |> _.Value |> Seq.length
        Assert.That(results, Is.EqualTo 5)