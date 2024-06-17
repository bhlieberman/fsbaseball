namespace Statcast

open System
open System.Text

/// Extension to Types.fs that contains search/filter metadata typedefs.
module MoreTypes =

    type GroupBy =
        | PlayerName
        | PlayerAndGame
        | Team
        | PlayerAndMonthAllYears
        | PlayerAndMonthSplitYears
        | PlayerAndYear
        | PlayerAndEvent
        | PlayerAndPitchType
        | TeamAndPitchType
        | TeamAndGame
        | TeamAndMonthAllYears
        | TeamAndMonthSplitYears
        | TeamAndYear
        | Venue
        | League
        | LeagueAndYear

    type SortOrder =
        | Desc
        | Asc

    type SortBy =
        | Pitches
        | PitchPercentage
        | BA of string
        | AvgPitchVelo of string
        | AvgArmSideMovement of string // possible candidate for Units of Measure?
        | AvgExitVelo of string
        | AvgStartDist1B of string

        static member getBA =
            function
            | (Some value: string option) -> BA value
            | _ -> BA ""

        static member validateBA(input: string option) =
            let instance = SortBy.getBA input

            let valid =
                match instance with
                | (BA inner) ->
                    Seq.contains
                        inner
                        [ "xBA"
                          "BA - xBA"
                          "wOBA"
                          "xwOBA"
                          "wOBA - xwOBA"
                          "SLG"
                          "xSLG"
                          "SLG - xSLG"
                          "OBP"
                          "xOBP"
                          "OBP - xOBP"
                          "Barrels"
                          "ISO"
                          "BABIP"
                          "Whiff Rate"
                          "Swings"
                          "Whiffs"
                          "Sum Batter Run Value"
                          "Sum Pitcher Run Value"
                          "Batter Run Value / 100 Pitches"
                          "Pitcher Run Value / 100 Pitches" ]
                | _ -> false

            if valid then
                instance
            else
                raise (ArgumentException "no match with provided value")

        member this.createQueryValue(input: string option) =
            let inner =
                match SortBy.validateBA input with
                | (BA inner) -> inner
                | _ -> ""

            let sb = new StringBuilder(inner)

            match inner with
            | "Batter Run Value / 100 Pitches"
            | "Pitcher Run Value / 100 Pitches"
            | "Sum Batter Run Value"
            | "Sum Pitcher Run Value" -> sb.Replace(" / ", "_per_").Replace(" ", "_").ToString().ToLower()
            | _ -> ""

        static member getAPV =
            function
            | (Some input: string option) -> AvgPitchVelo input
            | _ -> AvgPitchVelo ""

        static member validateAPV(input: string option) =
            let instance = SortBy.getAPV input

            let valid =
                match instance with
                | (AvgPitchVelo inner) ->
                    Seq.contains
                        inner
                        [ "Avg. Perceived Velocity"
                          "Avg. Spin Rate"
                          "Horizontal Release Point"
                          "Vertical Release Point"
                          "Avg. Release Extension"
                          "Avg. Plate Z"
                          "Avg. Plate X" ]

                | _ -> false

            if valid then
                instance
            else
                raise (ArgumentException "no match with provided value")

        static member getArmSideMvmt =
            function
            | (Some input) -> AvgArmSideMovement input
            | _ -> AvgArmSideMovement ""

        static member validateArmSideMvmt(input: string option) =
            let instance = SortBy.getArmSideMvmt input

            let valid =
                match instance with
                | (AvgArmSideMovement inner) ->
                    Seq.contains
                        inner
                        [ "Avg. Movement Toward Batter"
                          "Avg. Downward Movement w/ Gravity"
                          "Avg. Vertical Movement w/o Gravity" ]
                | _ -> false

            if valid then
                instance
            else
                raise (ArgumentException "no match with provided value")

        static member getAvgExitVelo =
            function
            | (Some input) -> AvgExitVelo input
            | _ -> AvgExitVelo ""

        static member validateAvgExitVelo(input: string option) =
            let instance = SortBy.getAvgExitVelo input

            let valid =
                match instance with
                | (AvgExitVelo inner) ->
                    Seq.contains
                        inner
                        [ "Avg. Adjusted Exit Velocity"
                          "Avg. Distance"
                          "Avg. Launch Angle"
                          "Bat Speed"
                          "Swing Length"
                          "Hard Hit %"
                          "Barrels/BBE%"
                          "Barrels/PA%" ]
                | _ -> false

            if valid then
                instance
            else
                raise (ArgumentException "no match with provided value")

        static member getStartDist1B =
            function
            | (Some input) -> AvgStartDist1B input
            | _ -> AvgStartDist1B ""

        static member validateStartDist1B(input: string option) =
            let instance = SortBy.getStartDist1B input

            let valid =
                match instance with
                | (AvgStartDist1B inner) ->
                    Seq.contains
                        inner
                        (Seq.map (fun s -> "Avg Start Distance " + s) [ "2B"; "3B"; "SS"; "LF"; "CF"; "RF" ])
                | _ -> false

            if valid then
                instance
            else
                raise (ArgumentException "no match with provided value")
