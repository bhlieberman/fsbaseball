namespace Statcast

open System
open System.Text.RegularExpressions

/// An extension to the built-in String type
/// that provides a simple method for converting
/// stringified record values to snake_case for use
/// in the Statcast URL.
module StringExtensions =
    let private words = new Regex("(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])")

    type String with
        member this.ToURLCase() =
            let lowerCased = words.Split(this) |> Seq.map (fun s -> s.ToLower()) |> Seq.toArray
            String.Join("_", lowerCased)

        member this.Slashes() = String.Join("/", words.Split(this))

        member this.DotDot() = String.Join("\.\.", words.Split(this))
