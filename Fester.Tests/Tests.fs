namespace Fester.Tests

open Xunit
open Fester.Request
open Helpers
open System.Net.Http

// see example explanation on xUnit.net website:
// https://xunit.github.io/docs/getting-started-dotnet-core.html
module Tests =

    let add x y = x + y

    [<Fact>]
    let ``Get creates message with GET verb`` () =
        TestRequest<string> 
            (fun (req:HttpRequestMessage) ->
                Assert.True(req.Method.Method = "GET"))
        |> Get "some/path" |> Raw |> ignore




