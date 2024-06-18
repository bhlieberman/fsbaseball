namespace Statcast

open System
open System.Text.RegularExpressions

/// An extension to the built-in String type
/// that provides a simple method for converting
/// stringified record values to snake_case for use
/// in the Statcast URL.
module StringExtensions =
    type String with
        member this.ToURLCase() =
            let words = Regex.Split(this, "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])")
            let lowerCased = words |> Seq.map (fun s -> s.ToLower()) |> Seq.toArray
            String.Join("_", lowerCased)

        member this.Slashes() =
            let words = Regex.Split(this, "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])")
            String.Join("/", words)

