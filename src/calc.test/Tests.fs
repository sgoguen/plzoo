module Tests

open System
open Xunit
open Calc

[<Fact>]
let ``My test`` () =
    let result = parse "2 + 3 - 2" |> Eval.eval
    Assert.StrictEqual(3, result)
    let result = parse "2 + 3 * 2" |> Eval.eval
    Assert.StrictEqual(8, result)
