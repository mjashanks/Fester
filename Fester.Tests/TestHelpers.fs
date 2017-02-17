namespace Fester.Tests

open Fester
open System.Net
open Newtonsoft.Json
open System.Threading.Tasks
open System.Net.Http

module Helpers =    

    type private NewtonsoftSerializer() = 
        interface ISerializer with
            member this.Serialize<'a> (ob:'a) =
                JsonConvert.SerializeObject ob
            member this.Deserialize<'a> str = 
                JsonConvert.DeserializeObject<'a> str
            member this.ContentType = "application/json"

    let private serializer = new NewtonsoftSerializer() :> ISerializer
    let private serialize<'a> (ob: option<'a>) =
        match ob with
        | None -> ""
        | Some o -> serializer.Serialize<'a> o   

    type SendMessageInterceptor<'a> = {
        OnSendMessage : HttpRequestMessage -> unit
        ResponseStatus : HttpStatusCode 
        ResponseContent : 'a Option
    }    

    let defaultInterceptor<'a> onSendMessage =
        {OnSendMessage = onSendMessage; ResponseStatus = HttpStatusCode.OK; ResponseContent = None}
    let private getSendRequest<'a> interceptor : SendRequest = 
        fun (r:HttpRequestMessage) -> 

            new Task<HttpResponseMessage>(
                fun () -> 
                    interceptor.OnSendMessage r
                    let response = new HttpResponseMessage ()
                    response.StatusCode <- interceptor.ResponseStatus
                    response.Content <- new StringContent( serialize (interceptor.ResponseContent) )
                    response
            )

    let CreateTestClient<'resp> (interceptor) = 
        let sendRequest = getSendRequest<'resp> interceptor
        new FesterClient(serializer, sendRequest)
        