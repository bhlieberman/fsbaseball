namespace Statcast

open FSharp.Data
open System
open SimpleTypes

module Player =

    let baseUrl = "https://baseballsavant.mlb.com/statcast_search/csv?"

    [<Literal>]
    let sampleUrl =
        "https://baseballsavant.mlb.com/statcast_search/csv?hfPT=CU|KC|CS|SL|ST|SV|KN|&hfAB=&hfGT=R|&hfPR=&hfZ=&hfStadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2024|&hfSit=&player_type=pitcher&hfOuts=&hfOpponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=2024-06-02&game_date_lt=2024-06-03&hfMo=&hfTeam=BAL|&home_road=&hfRO=&position=&hfInfield=&hfOutfield=&hfInn=&hfBBT=&hfFlag=&metric_1=&group_by=name&min_pitches=0&min_results=0&min_pas=0&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc#results"

    type Statcast = CsvProvider<sampleUrl>

    let isBigDateRange (lower: DateOnly, upper: DateOnly) : bool = upper.DayOfYear - lower.DayOfYear >= 42

    let partitionDateRange lower upper =
        Seq.unfold
            (fun (st: DateOnly) ->
                if st.Equals(upper) then
                    None
                else
                    Some(st.AddDays(1), st.AddDays(1)))
            lower
        |> Seq.chunkBySize 10
        |> Seq.map (fun l -> [| Seq.head l; Seq.last l |])
        |> Seq.concat
        |> Seq.pairwise // this isn't exactly what I want
        |> Seq.map GameDate.create

    let splitWork (qp: QueryParams) =
        match qp with
        | { QueryParams.gameDateGT = (GreaterThan start)
            QueryParams.gameDateLT = (LessThan stop) } when isBigDateRange <| (start, stop) ->
            partitionDateRange start stop
            |> Seq.map (fun (start, stop) ->
                { qp with
                    gameDateGT = start
                    gameDateLT = stop })
        | _ -> Seq.singleton qp

    let runStatcast (qp: QueryParams) =
        async {
            try
                let url = baseUrl + qp.ToQueryString()
                let! result = Statcast.AsyncLoad(url)
                for row in result.Rows do
                    Console.WriteLine row
            with
                | ex -> Console.WriteLine ex
        }

    let runAll (params': QueryParams seq) =
        params'
        |> Seq.map runStatcast
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
