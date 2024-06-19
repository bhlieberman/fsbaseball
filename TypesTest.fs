module baseball

open NUnit.Framework
open System
open Statcast.SimpleTypes
open Statcast.StringExtensions

[<TestFixture>]
type TypesTest() =
    [<Test>]
    member this.checkValidateInput() =
        let pitchStrings = [ "knuckleball"; "slow curve"; "slider"; "madeup" ]
        let validated = pitchStrings |> List.map (fun p -> PitchType.validateInput (Some p))
        Assert.That(List.length validated, Is.EqualTo(4))
        Assert.That(List.last validated, Is.EqualTo(NoInput))

    [<Test>]
    member this.checkPAResultFormatting() =
        let hits = [ Single; Double; Triple; HomeRun ]

        let outs =
            [ FieldOut
              Strikeout
              StrikeoutDoublePlay
              DoublePlay
              GIDP
              FieldersChoice
              FieldersChoiceOut
              ForceOut
              SacBunt
              SacBuntDoublePlay
              SacFly
              SacBuntDoublePlay
              TriplePlay ]

        let hitsFormatted =
            hits
            |> Seq.map (fun v -> v |> _.ToString() |> _.DotDot() |> _.ToLower())
            |> Seq.toArray

        Assert.AreEqual([| "single"; "double"; "triple"; "home\.\.run" |], hitsFormatted)

        let outsFormatted =
            outs
            |> Seq.map (fun v -> v |> _.ToString() |> _.DotDot() |> _.ToLower())
            |> Seq.take 4
            |> Seq.toArray

        Assert.AreEqual(
            [| "field\.\.out"
               "strikeout"
               "strikeout\.\.double\.\.play"
               "double\.\.play" |],
            outsFormatted
        )

    [<Test>]
    member _.checkGameType() =
        let gameTypes = [| WorldSeries; Wildcard; AllStar |]
        let gameTypesFormatted = gameTypes |> Seq.map _.ToString() |> Seq.toArray
        Assert.AreEqual([| "W"; "F"; "A" |], gameTypesFormatted)

    [<Test>]
    member _.checkGameDate() =
        let lessThanDate = new DateOnly(2021, 6, 3)
        let lessThan = LessThan lessThanDate
        let greaterThanDate = new DateOnly(2021, 6, 4)
        let greaterThan = GreaterThan greaterThanDate
        Assert.Less(lessThan, greaterThan) // maybe implement Compare?

    [<Test>]
    member _.checkMonth() =
        let may = May(new DateOnly(2024, 5, 1))
        let marApr = MarApr
        Assert.AreEqual([| "4"; "5" |], Array.map _.ToString() [| marApr; may |])

    [<Test>]
    member _.checkMonthFailure() =
        let badMay = May(new DateOnly(2024, 4, 2))
        let ex = Assert.Throws<ArgumentException>((fun _ -> badMay.ToString() |> ignore))
        Assert.That(ex.Message, Is.EqualTo("This date is not in May."))

    [<Test>]
    member _.checkHomeAway() =
        let home = Home "home"
        let away = Away "notAway"
        let actualHome = home.validateInput()
        let actualAway = away.validateInput()
        Assert.AreEqual(Some home, actualHome)
        Assert.AreNotEqual(Some away, actualAway)