namespace Fester

open System.Net
open System.Net.Http
open System.IO
open FSharp.Core
open System.Text
open System.Reflection
open System
open System.Net.Http.Headers

type ISerializer = 
    abstract member Serialize<'a> : 'a -> string
    abstract member Deserialize<'a> : string -> 'a
    abstract member ContentType : string

type FesterClient(serializer:ISerializer, baseUrl:string Option, createHttpClient) = 
    member this.Serializer = serializer
    member this.HttpClient : HttpClient = 
        let client : HttpClient = createHttpClient baseUrl   
        client.DefaultRequestHeaders
              .Accept
              .Add(new MediaTypeWithQualityHeaderValue(serializer.ContentType))
        client

module Client = 
    let private createHttpClient url =
        let client = new HttpClient()
        client.BaseAddress <- new Uri(match url with | None -> "" | Some s -> s)
        client
    let Create serializer baseUrl =
        new FesterClient(serializer, baseUrl, createHttpClient)


type Headers = (string * string) list

type Request = {
    Verb : HttpMethod
    Url : string
    Headers : Headers
    Body : string Option
    Client : FesterClient}

type ResponseWithException = {
    HttpResponse : HttpWebResponse
    Exception : Option<exn>}

type SuccessfulResponse<'resp> = {
    HttpResponse : HttpWebResponse
    Content : 'resp}

type FailedResponse = ResponseWithException

type Response<'resp> = 
    | Success of SuccessfulResponse<'resp>
    | Failed of FailedResponse


module Request = 

    let private methodWithBody (verb:HttpMethod) (url:string) (client:FesterClient) : Request = 
        {Verb=verb; Url=url; Headers=[]; Body=None; Client=client}

    let Get : string -> FesterClient -> Request = methodWithBody HttpMethod.Get
    let Post : string -> FesterClient -> Request = methodWithBody HttpMethod.Post
    let Put : string -> FesterClient -> Request  = methodWithBody HttpMethod.Put
    let Delete : string -> FesterClient -> Request  = methodWithBody HttpMethod.Delete
    let Body<'a> (ob:'a) (req:Request) : Request = 
        {req with Body=Some(req.Client.Serializer.Serialize ob)}
        
    let Header ((key:string),(value:string)) (req:Request) : Request = 
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

    let deserialize<'a> req str = 
        req.Client.Serializer.Deserialize<'a> str
        
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

    let private createRequest (request:Request) = 
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

    let private getHttpResponse (request:Request) : HttpResponseMessage = 
        let asyncResult = (createRequest request)
                          |> request.Client.HttpClient.SendAsync
        asyncResult.Result        

    let Raw (request:Request) = 
        request 
        |> getHttpResponse
        |> fun r -> r.Content

    let As<'resp> (request:Request) =
        request |> Raw |>  deserialize<'resp> request

    let GetResponse<'resp> (request:Request) : Response<'resp> = 
        let response = request |> getHttpResponse
        match response.HttpResponse.StatusCode with
        | HttpStatusCode.OK -> 
            Success {HttpResponse=response.HttpResponse; 
                     Content= (response.HttpResponse |> getResponseFromStream |> deserialize<'resp> request)}
        | _ ->
            Failed response
    
    let Cookie (cookie:string) (request:Request) =
        request |> Header ("Cookie", cookie)

    
    