namespace FSHARP_project_omega

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open WebSharper.Sitelets


[<JavaScript>]
module Client =

    type EndPoint =
        | [<EndPoint "/">] Home
        | [<EndPoint "/create">] Create
        | [<EndPoint "/edit">] Edit // Note: For specific item editing, consider /edit/{id}
        | [<EndPoint "/text-from-pdf">] TextFromPdf
        | [<EndPoint "/timer">] Timer
    
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    type Note = {
        Id: int
        Title: string
        Content: string // For actual rich text, this would need a different rendering approach
        Subject: string
        Tags: list<string>
    }

    module Pages =
        open WebSharper.UI.Html

        module Home =
            let People =
                ListModel.FromSeq [
                    "John"
                    "Paul"
                ]

            let NewName = Var.Create ""

        let HomePage() =
            IndexTemplate.HomePage()
                .ListContainer(
                    Home.People.View.DocSeqCached(fun (name: string) ->
                        IndexTemplate.ListItem().Name(name).Doc()
                    )
                )
                .Name(Home.NewName)
                .Add(fun e ->
                    Home.People.Add(Home.NewName.Value)
                    Home.NewName.Value <- ""
                )
                .Doc()
        
        let CreatePage() =
            IndexTemplate.CreatePage()
                .Doc()
        let EditPage() =
            IndexTemplate.EditPage()
                .Doc()
        
        let TextFromPdfPage() =
            IndexTemplate.TextFromPdfPage()
                .Doc()

        let TimerPage() =
            IndexTemplate.TimerPage()
                .Doc()
            
            
    let router = Router.Infer<EndPoint>()
    let currentPage = Router.InstallHash Home router

    type Router<'T when 'T: equality> with
        member this.LinkHash (ep: 'T) = "#" + this.Link ep
    
    [<SPAEntryPoint>]
    let Main () =
        let renderInnerPage (currentPage: Var<EndPoint>) =
            currentPage.View.Map (fun endpoint ->
                match endpoint with
                | Home        -> Pages.HomePage()
                | Create      -> Pages.CreatePage()
                | Edit        -> Pages.EditPage()
                | TextFromPdf -> Pages.TextFromPdfPage()
                | Timer       -> Pages.TimerPage()
            )
            |> Doc.EmbedView

        IndexTemplate()
            .Content(renderInnerPage currentPage)
            .SwitchToHome(fun _ ->
                currentPage.Value <- Home
            )
            .Bind()