namespace Statcast

open Player
open SimpleTypes
open System

module Scratch =

    let pitchTypes = [ CS; CU; FS ]
    let pitchTypes2 = FC
    let gameType = [ RegularSeason ]
    let gameDateLT = LessThan(new DateOnly(2024, 6, 17))
    let gameDateGT = GreaterThan(new DateOnly(2024, 6, 17))

    let qp =
        { pitchType = pitchTypes
          gameType = gameType
          gameDateLT = gameDateLT
          gameDateGT = gameDateGT }

    [<EntryPoint>]
    let main argv =
        runAll (splitWork qp)
        0
