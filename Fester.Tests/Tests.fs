namespace Fester.Tests

open Xunit
open Fester.Request
open Helpers
open System.Net.Http

// see example explanation on xUnit.net website:
// https://xunit.github.io/docs/getting-started-dotnet-core.html
module Tests =

    let assertMethodIs method (req:HttpRequestMessage) = 
        Assert.Equal(method, req.Method.Method)

    [<Fact>]
    let ``Get creates message with GET verb`` () =   
        assertMethodIs "GET"     
        |> TestRequest<string> |> Get "some/path" |> Raw |> ignore




