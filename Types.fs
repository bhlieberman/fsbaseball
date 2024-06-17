namespace Statcast

open System
open System.Text.RegularExpressions

open MoreTypes

/// This module contains the typed representation of the set of
/// parameters accepted by the Statcast endpoint.
module SimpleTypes =

    type PitchType =
        | CU
        | KC
        | CS
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
        | NoInput

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
                    | _ -> NoInput
            | _ -> NoInput

    type PAResult =
        | Single
        | Double
        | Triple
        | HomeRun
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
        | BaseHit of PAResult list
        | Outs of PAResult list

    type GameType =
        | RegularSeason
        | PostSeason
        | Playoffs
        | Wildcard
        | DivisionSeries
        | LeagueChampionship
        | WorldSeries
        | SpringTraining
        | AllStar

    type GameDate = // DONE
        | LessThan of DateOnly
        | GreaterThan of DateOnly

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

        override this.ToString() =
            match this with
            | (May date)
            | (Jun date)
            | (Jul date)
            | (Aug date) -> date.Month.ToString()
            | MarApr -> (new DateOnly(2024, 4, 1)).Month.ToString()
            | SepOct -> (new DateOnly(2024, 9, 1)).Month.ToString()

    type Team =
        | Orioles
        | BlueJays
        | Yankees
        | RedSox
        | Rays // TODO: add the rest

    /// Whether the team is playing at home or away.
    type HomeAway = // DONE
        | Home
        | Away
        | NoInput

        member _.validateInput() =
            function
            | (Some input: string option) ->
                let normalized = input.ToLower() in

                if Regex.Match(normalized, "^home$").Success then Home
                elif Regex.Match(normalized, "^away$").Success then Away
                else NoInput
            | _ -> NoInput

    type Stadium =
        { venue: (string * string) option }

        static member create =
            function
            | (abbrev: string, full: string) ->
                if (String.length abbrev) >= 2 then
                    { venue = Some(abbrev, full) }
                else
                    { venue = None }

    type PitchResult =
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

    type GamedayZones =
        { zones: int list }

        static member inZone = seq { 1..9 }
        static member outOfZone = seq { 11..14 }

    type BattedBallLocation =
        | Pitcher
        | Catcher
        | FirstBase
        | SecondBase
        | ThirdBase
        | ShortStop
        | LeftField
        | CenterField
        | RightField

    type AttackZones =
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
        { count: string option } // because plain type synonyms can't have member functions...

        static member create(balls: int, strikes: int) =
            if balls <= 3 && strikes <= 2 then
                Some(balls.ToString() + "-" + strikes.ToString())
            else
                None

    // member _.createStr: string list = // WIP
    //     function
    //     | "behind"
    //     | "ahead"
    //     | "2strikes"
    //     | "3balls"

    type Season =
        { value: int seq }

        static member allYears = seq { 2008..2024 }
        static member statcastYears = seq { 2015..2024 }
        static member pitchTracking = Season.allYears

    type Situation =
        | GoAheadPlate
        | GoAheadOnBase
        | TyingRunPlate
        | TyingRunOnBase
        | TyingRunOnDeck

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

    type Opponent = string list // ... or Team list

    type PitcherHandedness =
        | Left
        | Right

    type BatterHandedness =
        | Left
        | Right

    type QualityOfContact =
        | Barrel
        | SolidContact
        | FlareBurner
        | PoorlyUnder
        | PoorlyTopped
        | PoorlyWeak

    type RunnersOn =
        | NoRunners
        | RISP
        | RunnerOnBase
        | RunnerOnFirst of RunnersOn list
        | RunnerOnSecond of RunnersOn list
        | RunnerOnThird of RunnersOn list
        | RunnerNotOnFirst
        | RunnerNotOnSecond
        | RunnerNotOnThird

    type Position =
        | PlayerType
        | DH
        | Outfield
        | Infield

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
        | IsSweetSpot
        | PosPlayerPitching
        | RemoveBunts
        | StartingPosPlayer
        | NonStartingPosPlayer
        | IsRookieBatter
        | IsRookiePitcher
        | IsPassedBall
        | IsWildPitch
        | IsEV50Batter
        | IsEV50Pitcher
        | CompetitiveSwing
        | IsSword
        | IsBlast
        | IsSquaredUp

    type MinPA = int
    type MinResults = int
    type MinTotalPitches = int

    type QueryParams =
        { pitchType: PitchType list
          atBats: int
          gameType: GameType
          gameDateLT: GameDate
          gameDateGT: GameDate }

        member this.ToQueryString() =
            seq {
                yield!
                    seq {
                        Seq.fold2
                            (fun acc pt delim -> acc + pt.ToString() + delim)
                            ""
                            this.pitchType
                            (Seq.replicate (List.length this.pitchType) "|")
                    }

                yield ("&AB=" + this.atBats.ToString())
                yield ("&GT=" + this.gameType.ToString())
                yield ("&game_date_lt=" + this.gameDateLT.ToString())
                yield ("&game_date_gt=" + this.gameDateGT.ToString())
            }
            |> Seq.fold (fun acc s -> acc + s) ""
