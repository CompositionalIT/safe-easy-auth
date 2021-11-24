module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Todos: Todo list; Input: string; User : User option }

type AsyncOperation<'T, 'Q> = Request of 'T | Response of 'Q

type Msg =
    | SetInput of string
    | GetTodos of AsyncOperation<unit, Todo list>
    | AddTodo of AsyncOperation<unit, Todo>
    | GetUser of AsyncOperation<unit, User option>

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () =
    let model = { Todos = []; Input = ""; User = None }
    let commands = Cmd.batch [
        Cmd.ofMsg (GetTodos (Request ()))
        Cmd.ofMsg (GetUser (Request ()))
    ]
    model, commands

let update (msg: Msg) (model: Model) =
    match msg with
    | GetTodos (Request _) -> { model with Todos = [] }, Cmd.OfAsync.perform todosApi.getTodos () (Response >> GetTodos)
    | GetTodos (Response todos) -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo (Request _) ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo (Response >> AddTodo)
        { model with Input = "" }, cmd
    | AddTodo (Response todo) ->
        { model with
            Todos = model.Todos @ [ todo ] },
        Cmd.none
    | GetUser (Request _) ->
        let cmd = Cmd.OfAsync.perform todosApi.getUser () (Response >> GetUser)
        model, cmd
    | GetUser (Response user) ->
        { model with User = user }, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand model =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
        match model.User with
        | Some user ->
            Bulma.navbarItem.div [
                color.isSuccess
                prop.text $"Welcome, {user.FullName} ({user.Username})!"
            ]
        | None ->
            Bulma.navbarItem.div [
                Bulma.notification [
                    color.isWarning
                    prop.text $"No login details available. Is authentication turned off?"
                ]
            ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (SetInput >> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch (AddTodo (Request ())))
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand model ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "_1_easy_auth"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
