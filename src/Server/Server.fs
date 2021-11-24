module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.AspNetCore.Http
open Saturn

open Shared
open System
open Giraffe

module Storage =
    let todos = ResizeArray<_>()

    let getTodos() = List.ofSeq todos
    let addTodo(todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

Storage.addTodo (Todo.create "Create new SAFE project") |> ignore
Storage.addTodo (Todo.create "Write your app") |> ignore
Storage.addTodo (Todo.create "Ship it !!!") |> ignore

module Auth =
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.Hosting
    open Microsoft.Extensions.Logging
    open System.Security.Claims

    type UserClaim =
        {
            typ : string
            ``val`` : string
        }
    type MsClientPrincipal =
        {
            auth_typ : string
            claims : seq<UserClaim>
            name_typ : string
            role_typ : string
        }

    let xMsClientPrincipal = "X-MS-CLIENT-PRINCIPAL"

    let tryGetEasyAuthClientPrincipal (ctx:HttpContext) =
        ctx.Request.Headers.[xMsClientPrincipal]
        |> Seq.tryHead
        |> Option.map (
            Convert.FromBase64String
            >> Text.Encoding.Default.GetString
            >> Newtonsoft.Json.JsonConvert.DeserializeObject<MsClientPrincipal>)

    let trySetEasyAuthClaimsPrincipal (ctx:HttpContext) (logger:ILogger) =
        match tryGetEasyAuthClientPrincipal ctx with
        | Some clientPrincipal ->
            ctx.User <-
                let claimsPrincipal = ClaimsPrincipal()
                let claims = clientPrincipal.claims |> Seq.map (fun userClaim -> Claim(userClaim.typ, userClaim.``val``))
                claimsPrincipal.AddIdentity(ClaimsIdentity(claims, clientPrincipal.auth_typ, clientPrincipal.name_typ, clientPrincipal.role_typ))
                claimsPrincipal
            logger.LogInformation $"Updated EasyAuth claims principal with data from request header."
        | None ->
            let message = "Could not update EasyAuth claims principal."
            logger.LogError message
            failwith message

    let withAuthenticatedUser handler next (ctx:HttpContext) =
        let logger = ctx.GetService<ILogger<User>>()
        let env = ctx.GetService<IWebHostEnvironment>()
        if not (env.IsDevelopment()) then
            trySetEasyAuthClaimsPrincipal ctx logger
            if ctx.User.Identity.IsAuthenticated then
                logger.LogInformation $"Authenticated real user for non-development environment."
            else
                failwith "User not authenticated."
        handler next ctx

let todosApi (ctx:HttpContext) =
    {   getTodos = fun () -> async {
            return Storage.getTodos ()
        }
        addTodo = fun todo -> async {
            match Storage.addTodo todo with
            | Ok () -> return todo
            | Error e -> return failwith e
        }
        getUser = fun () -> async {
            return
                if ctx.User.Identity.IsAuthenticated then
                    Some { Username = ctx.User.Identity.Name
                           FullName = ctx.User.FindFirst "name" |> Option.ofObj |> Option.map (fun claim -> claim.Value) |> Option.defaultValue "" }
                else
                    None
        }
    }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromContext todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router (Auth.withAuthenticatedUser webApp)
        memory_cache
        use_static "public"
        use_gzip
    }

run app
