namespace FSHARP_project_omega

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open WebSharper.Sitelets
open System
open WebSharper.JavaScript.Dom


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

    let Notes = ListModel.Create (fun note -> note.Id) []
    let NextId = Var.Create 1

    let router = Router.Infer<EndPoint>()
    let currentPage = Router.InstallHash Home router

    type Router<'T when 'T: equality> with
        member this.LinkHash (ep: 'T) = "#" + this.Link ep

    module Pages =
        open WebSharper.UI.Html

        module Home =
            let renderNotes () =
                Notes.View.DocSeqCached(fun note ->
                    IndexTemplate.NoteItem()
                        .NotePreview(note.Title + ": " + 
                            if note.Content.Length > 100 then 
                                note.Content.Substring(0, 100) + "..." 
                            else 
                                note.Content)
                        .EditNote(fun _ -> 
                            // Navigation for edit will be implemented later
                            Console.Log("Edit note: " + string note.Id))
                        .DeleteNote(fun _ -> 
                            Notes.Remove(note))
                        .Doc()
                )

        module Create =
            let titleVar = Var.Create ""
            let contentVar = Var.Create ""
            
            let saveNote () =
                let title = titleVar.Value
                let content = contentVar.Value
                if not (String.IsNullOrWhiteSpace(title) || String.IsNullOrWhiteSpace(content)) then
                    let newNote = {
                        Id = NextId.Value
                        Title = title
                        Content = content
                        Subject = "General" // Default subject, can be changed later
                        Tags = [] // Tags can be added later
                    }
                    
                    // Add the new note to our collection
                    Notes.Add(newNote)
                    
                    // Increment the next ID
                    NextId.Value <- NextId.Value + 1
                    
                    // Reset form fields
                    titleVar.Value <- ""
                    contentVar.Value <- ""
                    
                    // Navigate back to home page
                    currentPage.Value <- Home
                    
                    JS.Alert("Note added successfully!")
                else
                    JS.Alert("Please fill out both title and content fields.")

        let HomePage() =
            let doc = IndexTemplate.HomePage()
            doc.NotesContainer(Home.renderNotes())
                .CreateNewNote(fun _ -> 
                    // Navigate to create page
                    currentPage.Value <- Create)
                .Doc()
        
        let CreatePage() =
            IndexTemplate.CreatePage()
                .SaveNote(fun _ ->
                    // Get current values from DOM
                    let title = (JS.Document.GetElementById("note-title") :?> HTMLInputElement).Value
                    let content = (JS.Document.GetElementById("note-content") :?> HTMLTextAreaElement).Value
                    
                    // Update the vars
                    Create.titleVar.Value <- title
                    Create.contentVar.Value <- content
                    
                    // Call save function
                    Create.saveNote()
                )
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