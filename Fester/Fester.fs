namespace Fester

open System.Net
open System.Net.Http
open System.IO
open FSharp.Core
open System.Text
open System.Reflection
open System
open System.Net.Http.Headers
open System.Threading.Tasks

type ISerializer = 
    abstract member Serialize<'a> : 'a -> string
    abstract member Deserialize<'a> : string -> 'a
    abstract member ContentType : string

type SendRequest =  (HttpRequestMessage -> Task<HttpResponseMessage>)

type FesterClient(serializer, sendRequest) = 
    member this.Serializer = serializer
    member this.SendRequest : SendRequest = sendRequest
    

module Client = 
    let private getSendRequest url contentType = 
        let client = new HttpClient()
        client.BaseAddress <- new Uri(match url with | None -> "" | Some s -> s)
        client.DefaultRequestHeaders
                .Accept
                .Add(new MediaTypeWithQualityHeaderValue(contentType))
        fun (r:HttpRequestMessage) -> r |> client.SendAsync
    
    let Create (serializer:ISerializer) (baseUrl:string Option) =
        let sendRequest = getSendRequest baseUrl (serializer.ContentType)
        new FesterClient(serializer, sendRequest)



type Headers = (string * string) list

type FesterRequest = {
    Verb : HttpMethod
    Url : string
    Headers : Headers
    Body : string Option
    Client : FesterClient}

type SuccessfulResponse<'resp> = {
    HttpResponse : HttpResponseMessage
    Content : 'resp}

type FesterResponse<'resp> = 
    | Success of SuccessfulResponse<'resp>
    | Failed of HttpResponseMessage


module Request = 

    let private methodWithBody (verb:HttpMethod) (url:string) (client:FesterClient) : FesterRequest = 
        {Verb=verb; Url=url; Headers=[]; Body=None; Client=client}

    let Get : string -> FesterClient -> FesterRequest = methodWithBody HttpMethod.Get
    let Post : string -> FesterClient -> FesterRequest = methodWithBody HttpMethod.Post
    let Put : string -> FesterClient -> FesterRequest  = methodWithBody HttpMethod.Put
    let Delete : string -> FesterClient -> FesterRequest  = methodWithBody HttpMethod.Delete
    let Body<'a> (ob:'a) (req:FesterRequest) : FesterRequest = 
        {req with Body=Some(req.Client.Serializer.Serialize ob)}
        
    let Header ((key:string),(value:string)) (req:FesterRequest) : FesterRequest = 
        {req with Headers = (req.Headers |> List.append [key,value])}

    let url root path = sprintf "%s/%s" root path
    let rec private createHeaderCollection (col:WebHeaderCollection) (headers:Headers) = 
        match headers with
        | [] -> col
        | (k,v)::tail -> 
            col.[k] <- v 
            match tail with
            | [] -> col
            | _ -> createHeaderCollection col tail

    let private deserialize<'a> req (content:HttpContent) = 
        (content.ReadAsStringAsync().Result)
        |> req.Client.Serializer.Deserialize<'a> 
        
    let rec removeTrailingAndLeadingSlash (str:string) = 
        match str.Trim() with
        | r when ((r.EndsWith "/") && (r.StartsWith "/")) -> 
            r.Substring(1, r.Length - 2) |> removeTrailingAndLeadingSlash
        | r when r.StartsWith "/" -> 
            r.Substring(1, r.Length - 1) |> removeTrailingAndLeadingSlash
        | r when r.EndsWith "/" -> 
            r.Substring(0, r.Length - 1) |> removeTrailingAndLeadingSlash
        | _ -> str
    
    let ConstructUrl (paths:list<string>) (query_params:Option<obj>) = 

        let constructUrlPath (paths:list<string>) =
            paths
            |> Seq.map removeTrailingAndLeadingSlash
            |> Seq.fold (fun acc el -> acc + "/" + el) ""  

        let constructQueryString (parms:obj) = 
            let objType = parms.GetType()
            objType.GetProperties()
            |> Seq.map (fun p -> (p.Name, p.GetValue(obj).ToString()))
            |> Seq.fold (fun acc el ->
                            let k,v = el
                            acc + (sprintf "&%s=%s" k v))
                        "?"

        match query_params with
        | None -> constructUrlPath paths
        | Some o -> (constructUrlPath paths) + (constructQueryString o)
     

    let private getClient req =
        req.Client

    let rec private addHeaders (headers:(string*string) list) (netRequest:HttpRequestMessage)  =
        match headers with
        | [] -> netRequest
        | (k,v)::tail -> 
            netRequest.Headers.Add(k,v)
            addHeaders tail netRequest

    let private getContent req = 
        match req.Body with
        | None -> ""
        | Some b -> b

    let private createRequest (request:FesterRequest) = 
        let webRequest = new HttpRequestMessage(request.Verb, request.Url)
        webRequest.Content <- new StringContent((request |> getContent)
                                                ,Encoding.UTF8
                                                ,request.Client.Serializer.ContentType)

        addHeaders (request.Headers) webRequest |> ignore
        webRequest

    let rawBody (resp : HttpWebResponse) = 
        let all = resp
                  |> fun r -> (new StreamReader(r.GetResponseStream())).ReadToEnd()
        let bodyStart = all.IndexOf("\n")
        all.Substring(bodyStart, all.Length - bodyStart - 1)
    

    let private getResponseFromStream (r:HttpWebResponse) = 
        (new StreamReader(r.GetResponseStream())).ReadToEnd()

    let private getHttpResponse (request:FesterRequest) : HttpResponseMessage = 
        let asyncResult = (createRequest request)
                          |> request.Client.SendRequest
        asyncResult.Result        

    let Raw (request:FesterRequest) = 
        (request |> getHttpResponse).Content        

    let As<'resp> (request:FesterRequest) =
        request |> Raw |>  deserialize<'resp> request

    let GetResponse<'resp> (request:FesterRequest) : FesterResponse<'resp> = 
        let response = request |> getHttpResponse
        match response.StatusCode with
        | HttpStatusCode.OK -> 
            Success {HttpResponse=response; 
                     Content= (response.Content |> deserialize<'resp> request)}
        | _ ->
            Failed response
    
    let Cookie (cookie:string) (request:FesterRequest) =
        request |> Header ("Cookie", cookie)
       