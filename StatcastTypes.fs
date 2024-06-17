namespace Statcast

module Types =

    type PitchType =
        | CU of string option
        | KC of string option
        | CS of string option
        | SL of string option
        | ST of string option
        | SV of string option
        | KN of string option
        | FF of string option
        | SI of string option
        | FC of string option
        | FS of string option
        | FO of string option
        | SC of string option
        | EP of string option
        | FA of string option
        | IN of string option
        | PO of string option
        | NoInput of string option

        member _.validateInput(input: string option) =
            match input with
            | (Some "curveball") -> CU
            | (Some "slider") -> SL
            | (Some "knuckleball") -> KN
            | (Some "fastball") -> FF
            | (Some "cutter") -> FC
            | (Some "sinker") -> SI
            | (Some "split-finger") -> FS
            | (Some "forkball") -> FO
            | (Some "screwball") -> SC
            | (Some "knuckle curve") -> KC
            | (Some "slow curve") -> CS
            | (Some "sweeper") -> ST
            | (Some "slurve") -> SV
            | (Some "eephus") -> EP
            | (Some "other") -> FA
            | (Some "intentional ball") -> IN
            | (Some "pitchout") -> PO
            | _ -> NoInput


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

    type GameDate =
        | LessThan of System.DateOnly
        | GreaterThan of System.DateOnly

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

    type Month =
        | MarApr
        | May
        | Jun
        | Jul
        | Aug
        | SepOct

    type Team =
        | Orioles
        | BlueJays
        | Yankees
        | RedSox
        | Rays // TODO: add the rest

    type HomeAway =
        | Home
        | Away

    type Stadium = Venue of (string * string)
    type Inning = int

    type MinPA = int
    type MinResults = int
    type MinTotalPitches = int

    type GroupBy =
        | PlayerName
        | PlayerAndGame
        | Team // TODO: add the rest

    type SortOrder =
        | Desc
        | Asc

    type SortBy =
        | Pitches
        | PitchPercentage // TODO: add all stats options

    type QueryParams =
        { pitchType: PitchType list
          atBats: int
          gameType: GameType }

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

                yield ("AB=" + this.atBats.ToString())
                yield ("GT=" + this.gameType.ToString())
            } // maybe use Seq.collect?
