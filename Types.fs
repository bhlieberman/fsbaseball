namespace Statcast

open System
open System.Text.RegularExpressions

open MoreTypes
open StringExtensions

/// This module contains the typed representation of the set of
/// parameters accepted by the Statcast endpoint. See https://www.mlb.com/glossary/statcast
/// for definitions of terms and constructors not identified explicitly in comments.
module SimpleTypes =
    // TODO: unless otherwise specified, remaining todos
    // will be to correctly implement ToString on each type.

    /// Represents the PT variable in the query string. Use one of the `select*`
    /// static methods to choose the pitch types you want. A method will be
    /// provided that creates a list of valid pitch types given one or more
    /// "canonical" names of pitches: e.g. "forkball", "slider"
    type PitchType =
        | CU
        | KC
        | CS
        | CH
        | SL
        | ST
        | SV
        | KN
        | FF
        | SI
        | FC
        | FS
        | FO
        | SC
        | EP
        | FA
        | IN
        | PO
        | Fastball of PitchType list
        | Offspeed of PitchType list
        | CurveballGroup of PitchType list
        | SliderGroup of PitchType list
        | Breaking of PitchType list
        | OtherPitches of PitchType list
        | NoInput

        static member selectFastballs = Fastball [ FF; SI; FC ]
        static member selectOffspeed = Offspeed [ CH; FS; FO; SC ]
        static member selectCurves = CurveballGroup [ CU; KC; CS ]
        static member selectSliders = SliderGroup [ SL; ST; SV ]

        static member selectBreaking =
            match (PitchType.selectCurves, PitchType.selectSliders) with
            | (CurveballGroup curves, SliderGroup sliders) -> List.append curves sliders
            | _ -> List.empty

        static member selectOthers = OtherPitches [ EP; FA; IN; PO ]


        static member validateInput =
            function
            | (Some input': string option) ->
                let normalized = input'.ToLower()
                let delimited = normalized.Contains("-") || normalized.Contains(" ")

                if delimited then
                    match normalized with
                    | "knuckle curve" -> KC
                    | "slow curve" -> CS
                    | "intentional ball" -> IN
                    | "split-finger" -> FS
                    | _somethingElse -> NoInput // TODO: handle weird cases
                else
                    match normalized with
                    | "curveball" -> CU
                    | "slider" -> SL
                    | "knuckleball" -> KN
                    | "fastball" -> FF
                    | "cutter" -> FC
                    | "sinker" -> SI
                    | "forkball" -> FO
                    | "screwball" -> SC
                    | "sweeper" -> ST
                    | "slurve" -> SV
                    | "eephus" -> EP
                    | "other" -> FA
                    | "pitchout" -> PO
                    | _ -> NoInput // TODO: think about replacing this with None
            | _ -> NoInput

    /// All cases that are space-delimited in English (here in PascalCase)
    /// are separated by '\\.\\.' in the query string.
    type PAResult = // corresponded to hfAB parameter
        | Single // hits 1
        | Double // hits 2
        | Triple // hits 3
        | HomeRun // hits 4 -- "home\.\.run" in query string
        | FieldOut
        | Strikeout
        | StrikeoutDoublePlay
        | Walk
        | DoublePlay
        | FieldError
        | GIDP
        | FieldersChoice
        | FieldersChoiceOut
        | BatterInterference
        | CatcherInterference
        | CaughtStealing2B
        | CaughtStealing3B
        | CaughtStealingHome
        | ForceOut
        | HitByPitch
        | IntentionalWalk
        | SacBunt
        | SacBuntDoublePlay
        | SacFly
        | SacFlyDoublePlay
        | TriplePlay
        | BaseHit of PAResult list // TODO: write test cases for these recursive type constructors
        | Outs of PAResult list

    type GameType = // pipe-delimited type
        | RegularSeason
        | PostSeason
        | Playoffs
        | Wildcard
        | DivisionSeries
        | LeagueChampionship
        | WorldSeries
        | SpringTraining
        | AllStar

        override this.ToString() =
            match this with
            | RegularSeason -> "R"
            | Playoffs -> "PO"
            | DivisionSeries -> "D"
            | LeagueChampionship -> "L"
            | SpringTraining -> "S"
            | WorldSeries -> "W"
            | Wildcard -> "F"
            | AllStar -> "A"
            | _ -> ""

    type GameDate = // DONE
        | LessThan of DateOnly
        | GreaterThan of DateOnly

        static member private create'(lt: DateOnly, gt: DateOnly) =
            let lt' =
                if lt <= gt then
                    LessThan lt
                else
                    failwith "You provided a lower bound that exceeds the upper bound."

            let gt' = GreaterThan gt
            (lt', gt')

        static member create(lt: DateOnly, gt: DateOnly) = GameDate.create' (lt, gt)

        override this.ToString() =
            match this with
            | (LessThan v)
            | (GreaterThan v) -> v.ToString(@"yyyy-MM-dd")

    type Month =
        | MarApr // coded as 4
        | May of DateOnly
        | Jun of DateOnly
        | Jul of DateOnly
        | Aug of DateOnly
        | SepOct // coded as 9

        static member create(d: DateOnly) =
            match d.Month with
            | 4 -> MarApr
            | 5 -> May d
            | 6 -> Jun d
            | 7 -> Jul d
            | 8 -> Aug d
            | 9 -> SepOct
            | _ -> raise <| ArgumentException "The date provided is not within a typical baseball season."

        override this.ToString() =
            match this with
            | (May date) | (Jun date) | (Jul date) | (Aug date) -> date.Month.ToString()
            | MarApr -> (new DateOnly(2024, 4, 1)).Month.ToString()
            | SepOct -> (new DateOnly(2024, 9, 1)).Month.ToString()

    type Team = // I think these need to handled like...
        | Orioles // BAL
        | BlueJays // TOR
        | Yankees // NYY
        | RedSox // BOS
        | Rays // not sure
        | Guardians // CLE
        | Royals // KC
        | Tigers // DET
        | Twins // MIN
        | WhiteSox // CWS
        | Angels // LAA
        | Astros // HOU
        | Athletics // OAK
        | Mariners // SEA
        | Rangers // TEX
        | Braves // ATL
        | Marlins // MIA
        | Mets // NYM
        | Nationals // WAS
        | Phillies // PHI
        | Brewers // MIL
        | Cardinals // STL
        | Cubs // CHC
        | Pirates // PIT
        | Reds // CIN
        | DBacks // AZ
        | Dodgers // LAD
        | Giants // SF
        | Padres // SD
        | Rockies // COL
        | AmericanLeague of Team list
        | NationalLeague of Team list

        static member teamAbbrevs = Map.ofList [
            (Orioles, "BAL")
            (BlueJays, "TOR")
            (Yankees, "NYY")
            (Rays, "TB")
            (RedSox, "BOS")
            (Guardians, "CLE")
            (WhiteSox, "CWS")
            (Royals, "KC")
            (Tigers, "DET")
            (Twins, "MIN")
            (Angels, "LAA")
            (Athletics, "OAK")
            (Mariners, "SEA")
            (Rangers, "TEX")
            (Astros, "HOU")
            (Braves, "ATL")
            (Marlins, "MIA")
            (Mets, "NYM")
            (Nationals, "WAS")
            (Phillies, "PHI")
            (Brewers, "MIL")
            (Cardinals, "STL")
            (Cubs, "CHC")
            (Pirates, "PIT")
            (Reds, "CIN")
            (DBacks, "AZ")
            (Dodgers, "LAD")
            (Rockies, "COL")
            (Padres, "SD")
            (Giants, "SF")
        ]

        static member americanLeague =
            AmericanLeague
                [ Orioles
                  BlueJays
                  RedSox
                  Yankees
                  Rays
                  Guardians
                  Royals
                  Tigers
                  Twins
                  WhiteSox
                  Angels
                  Astros
                  Athletics
                  Mariners
                  Rangers ]

        static member nationalLeague =
            NationalLeague
                [ Braves
                  Marlins
                  Mets
                  Nationals
                  Phillies
                  Brewers
                  Cardinals
                  Cubs
                  Pirates
                  Reds
                  DBacks
                  Dodgers
                  Giants
                  Padres
                  Rockies ]

    /// Whether the team is playing at home or away.
    type HomeAway = // DONE
        | Home of string
        | Away of string

        member this.validateInput() = // shouldn't this be static?
            match this with
            | (Home input)
            | (Away input) ->
                let normalized = input.ToLower().Trim() in

                if Regex.Match(normalized, "^home$").Success then
                    Some this
                elif Regex.Match(normalized, "^away$").Success then
                    Some this
                else
                    None

    type Stadium =
        { venue: (string * string) option }
        // TODO: determine if it's worthwhile to
        // extend the mapping to use the integer values
        // for the stadiums. It is not alphabetical.
        // This could be quite tedious to reproduce·
        static member private stadiums =
            Map.ofList
                [ ("AZ", "Chase Field")
                  ("ATL", "Truist Park")
                  ("ATL-2016", "Turner Field")
                  ("BAL", "Oriole Park")
                  ("BOS", "Fenway Park")
                  ("CHC", "WrigleyField")
                  ("CIN", "GABP")
                  ("CLE", "Progressive Field")
                  ("COL", "Coors Field")
                  ("CWS", "Guaranteed Rate Fld")
                  ("DET", "Comerica Park")
                  ("FLA-2011", "Hard Rock Stadium")
                  ("HOU", "Minute Maid Park")
                  ("KC", "Kaufman Stadium")
                  ("LAA", "Angel Stadium")
                  ("LAD", "Dodger Stadium")
                  ("MIA", "Marlins Park")
                  ("MIL", "American Family Field")
                  ("MIN", "Target Field")
                  ("MIN-2009", "Metrodome")
                  ("NYM", "Citi Field")
                  ("NYM-2008", "Shea Stadium")
                  ("NYY", "Yankee Stadium")
                  ("OAK", "Oakland Coliseum")
                  ("PHI", "Citizens Bank Park")
                  ("PIT", "PNC Park")
                  ("SD", "Petco Park")
                  ("SEA", "T-Mobile Park")
                  ("SF", "Oracle Park")
                  ("STL", "Busch Stadium")
                  ("TB", "Tropicana Field")
                  ("TEX", "Globe Life Field")
                  ("TEX-2019", "Globe Life Park")
                  ("TOR", "Rogers Centre")
                  ("WSH", "Nationals Park") ]

        static member private create(abbrev: string, full: string) =
            if (String.length abbrev) >= 2 && String.length full > 0 then
                { venue = Some(abbrev, full) }
            else
                { venue = None }

        static member fromAbbrevOrFullName(?abbrev, ?fullName) =
            let abbrev' =
                match abbrev with
                | (Some chars: string option) ->
                    match Stadium.stadiums.TryFind(chars.ToUpper()) with
                    | (Some v) -> v
                    | None ->
                        failwith
                            $"""You entered: {chars}. This key does not exist.
                            Consult the docstring for valid key names."""
                | None -> ""

            let fullName' =
                match fullName with
                | (Some name: string option) ->
                    let reversed =
                        Map.keys Stadium.stadiums |> Seq.zip (Map.values Stadium.stadiums) |> Map.ofSeq

                    match Map.tryFind name reversed with
                    | (Some v) -> v
                    | None ->
                        failwith
                            $"""The full name you entered, {fullName}, did not match any existing value.
                            Consult the docstring for a list of valid full stadium names."""
                | _ -> ""

            Stadium.create (abbrev', fullName')



    type PitchResult = // all cases covered.
        // TODO: handle value.ToString()
        | Ball
        | BallInDirt
        | CalledStrike
        | Foul
        | FoulBunt
        | FoulTipBunt
        | FoulPitchout
        | Pitchout
        | HitByPitch
        | IntentionalBall
        | InPlay
        | MissedBunt
        | FoulTip
        | SwingingPitchout
        | SwingingStrike
        | SwingingStrikeBlocked
        | AllSwings of PitchResult list
        | SwingAndMiss of PitchResult list

        static member allSwings =
            AllSwings
                [ Foul
                  FoulBunt
                  FoulTipBunt
                  FoulPitchout
                  InPlay
                  MissedBunt
                  FoulTip
                  SwingingStrike
                  SwingingStrikeBlocked ]

        static member swingAndMiss = SwingAndMiss [ FoulTip; SwingingStrike; SwingingStrikeBlocked ]

    type GamedayZones = // hfZ -- pipe-delimited ints
        { zones: int list }

        static member inZone = seq { 1..9 }
        static member outOfZone = seq { 11..14 }

    type BattedBallLocation = // all cases covered.
        // TODO: handle value.ToString()
        | Pitcher
        | Catcher
        | FirstBase
        | SecondBase
        | ThirdBase
        | ShortStop
        | LeftField
        | CenterField
        | RightField

    type AttackZones = // hfNewZones -- pipe-delimited ints
        { zones: int list }

        static member heart = seq { 1..9 }
        static member shadow = seq { 11..19 }
        static member chase = seq { 21..29 }
        static member waste = seq { 31..39 }

    type BattedBallDirection =
        | Pull
        | Straightaway
        | Opposite

    type Count =
        { count: string option }

        static member create(balls: int, strikes: int) =
            if balls <= 3 && strikes <= 2 then
                { count = Some(balls.ToString() + "-" + strikes.ToString()) }
            else
                { count = None }

    type Season =
        { value: int seq }

        static member allYears = seq { 2008..2024 }
        static member statcastYears = seq { 2015..2024 }
        static member pitchTracking = Season.allYears

    type Situation =
        | GoAheadPlate
        | GoAheadOnBase
        | TyingRunAtPlate
        | TyingRunOnBase
        | TyingRunOnDeck

        override this.ToString() =
            match this with
            | GoAheadPlate -> "Go\.\.Ahead\.run\.at\.plate"
            | GoAheadOnBase -> "Go\.\.Ahead\.run\.on\.base"
            | TyingRunAtPlate
            | TyingRunOnBase
            | TyingRunOnDeck -> this.ToString().DotDot()


    type PlayerType =
        | Pitcher
        | Batter
        | Catcher
        | First
        | Second
        | Third
        | SS
        | CF
        | LF
        | RF

    type Outs = int list

    type Opponent = Team list

    type Handedness =
        | Left // L
        | Right // R

    let mkHandedness (s: string) =
        match s.Trim().ToLower() with
        | "left"
        | "l" -> Left
        | "right"
        | "r" -> Right
        | _ -> failwith "Please provide one of the following values: 'left', 'L', 'right', 'R'"

    type QualityOfContact =
        | Barrel
        | SolidContact
        | FlareBurner
        | PoorlyUnder
        | PoorlyTopped
        | PoorlyWeak

        override this.ToString() =
            match this with
            | FlareBurner
            | PoorlyTopped
            | PoorlyUnder
            | PoorlyWeak -> this.ToString().Slashes()
            | _ -> this.ToString()

    type RunnersOn =
        // TODO: maybe try to add the ability
        // to search for players on each base by name,
        // as provided by API.
        | NoRunners
        | RISP
        | RunnerOnBase
        | RunnerOnFirst of RunnersOn list // [1; 6; 8]
        | RunnerOnSecond of RunnersOn list // [2; 5; 8]
        | RunnerOnThird of RunnersOn list // [3; 6; 7]
        | RunnerNotOnFirst // 6
        | RunnerNotOnSecond // 7
        | RunnerNotOnThird // 8

        static member getRunnerOnFirst = RunnerOnFirst [ RunnerNotOnSecond; RunnerNotOnThird ]

        static member getRunnerOnSecond = RunnerOnSecond [ RunnerNotOnFirst; RunnerNotOnThird ]

        static member getRunnerOnThird = RunnerOnThird [ RunnerNotOnFirst; RunnerNotOnSecond ]

    type Position =
        | P // 1
        | C // 2
        | FirstBase // 3
        | SecondBase // 4
        | ThirdBase // 5
        | SS // 6
        | LF // 7
        | CF // 8
        | RF // 9
        | DH // 10
        | SP // starting pitcher
        | RP // relief pitcher
        | Outfield // OF
        | Infield // IF

    // TODO: implement type MetricRange

    type InfieldAlignment =
        | Standard
        | Strategic
        | Shift
        | Shade
        | Multiple of InfieldAlignment list

    type OutfieldAlignment =
        | Standard
        | Strategic
        | Other of string
        | Multiple of OutfieldAlignment list

    type Inning =
        { value: string list }

        static member allInnings =
            seq {
                yield! { 1..9 } |> Seq.fold (fun acc n -> acc + n.ToString()) ""
                yield! "extra innings"
            }

    type BattedBallType =
        | FlyBall
        | PopUp
        | LineDrive
        | GroundBall

    type Flags =
        | IsSwing
        | IsPA
        | IsTrackedPitch
        | IsBattedBall
        | IsTake
        | IsPutout
        | IsBasehit
        | IsInsideParkHR
        | IsOutsideParkHR
        | IsHardHit
        | IsBunt
        | IsLastPitch
        | IsSweetSpot // https://www.mlb.com/glossary/statcast/sweet-spot
        | PosPlayerPitching
        | RemoveBunts
        | StartingPosPlayer
        | NonStartingPosPlayer
        | IsRookieBatter
        | IsRookiePitcher
        | IsPassedBall
        | IsWildPitch
        | IsEV50Batter // average of hardest 50% of batted balls
        | IsEV50Pitcher // average of softest 50% of pitches
        | CompetitiveSwing
        | IsSword // https://www.mlb.com/glossary/statcast/sword
        | IsBlast // https://www.mlb.com/glossary/statcast/bat-tracking-blasts
        | IsSquaredUp

    type MinPA = int
    type MinPitches = int
    type MinResults = int
    type MinTotalPitches = int

    let mkPipeDelim (ls: 'a list) : string =
        (String.Empty, ls, (Seq.replicate (List.length ls) "|"))
        |||> Seq.fold2 (fun acc item delim -> acc + item.ToString() + delim)


    type QueryParams =
        { pitchType: PitchType list
          gameType: GameType list
          gameDateLT: GameDate
          gameDateGT: GameDate }

        member this.ToQueryString() =
            seq {
                yield ("hfPT=" + mkPipeDelim this.pitchType)
                yield ("&hfGT=" + mkPipeDelim this.gameType)
                yield ("&game_date_lt=" + this.gameDateLT.ToString())
                yield ("&game_date_gt=" + this.gameDateGT.ToString())
            }
            |> Seq.fold (fun acc s -> acc + s) ""
