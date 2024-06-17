namespace Statcast

open FSharp.Data

module Scratch =

    let baseUrl = "https://baseballsavant.mlb.com/statcast_search/csv?"

    [<Literal>]
    let sampleUrl =
        "https://baseballsavant.mlb.com/statcast_search/csv?hfPT=CU|KC|CS|SL|ST|SV|KN|&hfAB=&hfGT=R|&hfPR=&hfZ=&hfStadium=&hfBBL=&hfNewZones=&hfPull=&hfC=&hfSea=2024|&hfSit=&player_type=pitcher&hfOuts=&hfOpponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=2024-06-02&game_date_lt=2024-06-03&hfMo=&hfTeam=BAL|&home_road=&hfRO=&position=&hfInfield=&hfOutfield=&hfInn=&hfBBT=&hfFlag=&metric_1=&group_by=name&min_pitches=0&min_results=0&min_pas=0&sort_col=pitches&player_event_sort=api_p_release_speed&sort_order=desc#results"

    type Statcast = CsvProvider<sampleUrl>

    let sampleQuery = 
        let queryString = "hfPT=SL|&game_date_lt=2024-06-02&game_date_gt=2024-06-02"
        in (baseUrl + queryString) 
        |> Statcast.Load