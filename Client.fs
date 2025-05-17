namespace FSHARP_project_omega

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open WebSharper.Sitelets
open System


[<JavaScript>]
module Client =    
    type EndPoint =
        | [<EndPoint "/">] Home
        | [<EndPoint "/create">] Create
        | [<EndPoint "/open/{Id}">] Open of Id: int
        | [<EndPoint "/timer">] Timer
    
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    type Tag = {
        Id: int
        Title: string
        Color: string
    }

    let Tags = ListModel.Create (fun tag -> tag.Id) []
    let NextTagId = Var.Create 1

    type Note = {
        Id: int
        Title: string
        Content: string
        Subject: string
        Tags: list<Tag>
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
            let htmlToPlainText (html: string) =
                let tempDiv = JS.Document.CreateElement("div")
                tempDiv.InnerHTML <- html
                
                let plainText = tempDiv.TextContent
                
                if isNull plainText then "" else plainText

            let renderNotes () =
                Notes.View.DocSeqCached(fun note ->
                    IndexTemplate.NoteItem()
                        .NoteTitle(note.Title)
                        .NoteContent(
                            let plainTextContent = htmlToPlainText note.Content
                            if plainTextContent.Length > 300 then 
                                plainTextContent.Substring(0, 100) + "..." 
                            else 
                                plainTextContent)
                        .OpenNote(fun _ -> 
                            currentPage.Value <- Open note.Id)
                        .DeleteNote(fun _ -> 
                            Notes.Remove(note))
                        .Doc()
                )

        module Timer =
            let mutable private timerIntervalId : option<WebSharper.JavaScript.JS.Handle> = None
            let mutable private startTime : float = 0.0
            let mutable private remainingSeconds : float = 25.0 * 60.0 // Default to 25 minutes
            let mutable private totalSeconds : float = 25.0 * 60.0
            let mutable private isRunning : bool = false
            
            let timerMinutesVar = Var.Create 25
            let timerDisplayVar = Var.Create "25:00"
            let progressVar = Var.Create 100.0
            let isRunningVar = Var.Create false

            // Create audio element for alert sound
            let alertSound = JS.Document.CreateElement("audio") :?> HTMLAudioElement
            do
                alertSound.Src <- "https://assets.coderrocketfuel.com/pomodoro-times-up.mp3"
                alertSound.Load()

            let formatTime (seconds: float) =
                let mins = Math.Floor(seconds / 60.0)
                let secs = Math.Floor(seconds % 60.0)
                sprintf "%d:%02d" (int mins) (int secs)
                
            let updateDisplay() =
                timerDisplayVar.Value <- formatTime remainingSeconds
                progressVar.Value <- remainingSeconds / totalSeconds * 100.0

            let stopTimer() =
                match timerIntervalId with
                | Some id ->
                    JS.ClearInterval(id)
                    timerIntervalId <- None
                | None -> ()
                    
                isRunning <- false
                isRunningVar.Value <- false
                
            let playAlert() =
                alertSound.CurrentTime <- 0.0
                let _ = alertSound.Play()
                JS.Alert("Timer completed!")

            let startTimer() =
                if not isRunning then
                    isRunning <- true
                    isRunningVar.Value <- true
                    startTime <- Date.Now()
                    
                    // If timer was previously stopped and at 0, reset it
                    if remainingSeconds <= 0.0 then
                        let mins = timerMinutesVar.Value
                        remainingSeconds <- float mins * 60.0
                        totalSeconds <- remainingSeconds
                        updateDisplay()
                    
                    let callback() =
                        let now = Date.Now()
                        let elapsed = (float(now) - startTime) / 1000.0
                        startTime <- now
                        
                        remainingSeconds <- max 0.0 (remainingSeconds - elapsed)
                        updateDisplay()
                        
                        if remainingSeconds <= 0.0 then
                            stopTimer()
                            playAlert()
                    
                    timerIntervalId <- Some((JS.SetInterval callback) 1000)

            let resetTimer() =
                stopTimer()
                let mins = timerMinutesVar.Value
                remainingSeconds <- float mins * 60.0
                totalSeconds <- remainingSeconds
                updateDisplay()
                
            let toggleTimer() =
                if isRunning then stopTimer() else startTimer()
                
            let setMinutes(mins: int) =
                let validMins = max 1 (min 120 mins)
                timerMinutesVar.Value <- validMins
                
                // Only update remaining time if timer is not running
                if not isRunning then
                    remainingSeconds <- float validMins * 60.0
                    totalSeconds <- remainingSeconds
                    updateDisplay()

        module Create =
            let titleVar = Var.Create ""
            let contentVar = Var.Create ""
            
            let initRichTextEditor() =
                let editorToolbar = JS.Document.QuerySelectorAll(".editor-toolbar .toolbar-btn")
                let editorButtons = [| for i in 0 .. editorToolbar.Length - 1 -> editorToolbar.[i] :?> HTMLElement |]
        
                for btn in editorButtons do
                    btn.AddEventListener("click", fun (e: Dom.Event) ->
                        let target = e.Target :?> HTMLElement
                        let button = 
                            if target.TagName.ToLower() = "i" then 
                                target.ParentElement :?> HTMLElement 
                            else 
                                target
                        
                        let command = button.GetAttribute("data-command")
                        
                        try
                            match command with
                            | "bold" -> JS.Document.ExecCommand("bold", false, null) |> ignore
                            | "italic" -> JS.Document.ExecCommand("italic", false, null) |> ignore
                            | "underline" -> JS.Document.ExecCommand("underline", false, null) |> ignore
                            | "createLink" -> 
                                let url = JS.Window.Prompt("Enter URL:", "https://")
                                if not (isNull url) && url <> "" then
                                    JS.Document.ExecCommand("createLink", false, url) |> ignore
                            | "insertImage" ->
                                let url = JS.Window.Prompt("Enter image URL:", "https://")
                                if not (isNull url) && url <> "" then
                                    JS.Document.ExecCommand("insertImage", false, url) |> ignore
                            | _ -> 
                                JS.Document.ExecCommand(command, false, null) |> ignore

                        with _ ->
                            Console.Log("Command execution failed: " + command)
                        
                        let editor = JS.Document.GetElementById("note-content")
                        (editor :?> HTMLElement).Focus()
                        
                        e.PreventDefault()
                    )
        
                Console.Log("Rich text editor initialized")

            let saveNote () =
                let title = titleVar.Value
                let contentElement = JS.Document.GetElementById("note-content")
                let content = contentElement.InnerHTML
                
                if not (String.IsNullOrWhiteSpace(title) || String.IsNullOrWhiteSpace(content)) then
                    let newNote = {
                        Id = NextId.Value
                        Title = title
                        Content = content
                        Subject = "General"
                        Tags = []
                    }
                    
                    Notes.Add(newNote)
                    
                    NextId.Value <- NextId.Value + 1
                    
                    titleVar.Value <- ""
                    contentVar.Value <- ""
                    contentElement.InnerHTML <- ""
                    
                    currentPage.Value <- Home
                    
                    JS.Alert("Note added successfully!")
                else
                    JS.Alert("Please fill out both title and content fields.")

        let HomePage() =
            IndexTemplate.HomePage()
                .NotesContainer(Home.renderNotes())
                .CreateNewNote(fun _ -> 
                    currentPage.Value <- Create)
                .Doc()
        let CreatePage() =
            let doc = IndexTemplate.CreatePage()
            
            JS.Window.SetTimeout(fun () -> 
                Create.initRichTextEditor()
                Console.Log("Rich Text Editor initialized on page load")
            , 100) |> ignore
    
            doc.SaveNote(fun _ ->
                let title = (JS.Document.GetElementById("note-title") :?> HTMLInputElement).Value
                let content = JS.Document.GetElementById("note-content").InnerHTML
                
                Create.titleVar.Value <- title
                Create.contentVar.Value <- content
                
                Create.saveNote()
            )
        let OpenPage(id: int) =
            match Notes.TryFindByKey(id) with
            | Some note ->
                let doc = IndexTemplate.CreatePage()

                // Change the page header to 'Edit Note' after DOM is ready
                JS.Window.SetTimeout((fun () ->
                    // Set the header title and subtitle
                    let header = JS.Document.QuerySelector(".page-header .title-container h1")
                    if not (isNull header) then header.TextContent <- "Edit Note"
                    let subtitle = JS.Document.QuerySelector(".page-header .title-container p")
                    if not (isNull subtitle) then subtitle.TextContent <- "View or update your note."

                    // Initialize the rich text editor
                    Create.initRichTextEditor()

                    // Set form values
                    let titleElement = JS.Document.GetElementById("note-title") :?> HTMLInputElement
                    let contentElement = JS.Document.GetElementById("note-content")
                    if not (isNull titleElement) then titleElement.Value <- note.Title
                    if not (isNull contentElement) then contentElement.InnerHTML <- note.Content

                    // Update the vars
                    Create.titleVar.Value <- note.Title
                    Create.contentVar.Value <- note.Content
                ), 100) |> ignore

                // Save handler for updating existing note
                doc.SaveNote(fun _ ->
                    let titleElement = JS.Document.GetElementById("note-title") :?> HTMLInputElement
                    let contentElement = JS.Document.GetElementById("note-content")
                    let title = if isNull titleElement then "" else titleElement.Value
                    let content = if isNull contentElement then "" else contentElement.InnerHTML
                    let updatedNote = {
                        Id = note.Id
                        Title = title
                        Content = content
                        Subject = note.Subject
                        Tags = note.Tags
                    }
                    Notes.RemoveByKey(note.Id)
                    Notes.Add(updatedNote)
                    currentPage.Value <- Home
                    JS.Alert("Note updated successfully!")
                ) |> ignore

                // Cancel handler
                doc.SwitchToHome(fun _ ->
                    currentPage.Value <- Home
                ) |> ignore

                doc
            | None ->
                JS.Alert("Note not found!")
                currentPage.Value <- Home
                IndexTemplate.CreatePage()

        let TimerPage() =
            // Create the start button
            let startButton =
                let btn = JS.Document.CreateElement("button")
                btn.ClassName <- "btn timer-btn start-btn"
                
                // Handle the dynamic button appearance and behavior
                let updateButtonState isRunning =
                    btn.InnerHTML <- ""
                    
                    // Add icon
                    let icon = JS.Document.CreateElement("i")
                    icon.ClassName <- if isRunning then "fas fa-pause" else "fas fa-play"
                    btn.AppendChild(icon) |> ignore
                    
                    // Add text
                    let span = JS.Document.CreateElement("span")
                    span.TextContent <- if isRunning then " Pause" else " Start"
                    btn.AppendChild(span) |> ignore
                    
                    // Update button style
                    btn.ClassName <- if isRunning then 
                                        "btn timer-btn start-btn pause" 
                                     else 
                                        "btn timer-btn start-btn"
                
                // Set initial state
                updateButtonState false
                
                // Add click handler
                btn.AddEventListener("click", fun (e: Dom.Event) -> 
                    Timer.toggleTimer()
                )
                
                // Update button on timer state changes
                View.Sink updateButtonState Timer.isRunningVar.View |> ignore
                
                // Convert DOM element to Doc for template binding
                Doc.Static btn
            
            // Create a reactive string view for the progress width
            let progressWidthView = 
                Timer.progressVar.View
                |> View.Map (fun percent -> string percent + "%")
                
            // Create the progress bar as Doc element directly
            let progressBarDoc = 
                div [attr.``class`` "progress-bar"
                     Attr.DynamicStyle "width" progressWidthView] []

            // Initialize timer
            Timer.resetTimer()

            // Bind the template with proper Doc elements
            IndexTemplate.TimerPage()
                .TimerDisplay(Timer.timerDisplayVar.View)
                .StartButton(startButton)  // Pass the Doc element directly
                .TimerProgressBar(progressBarDoc)  // Removed unnecessary cast to Doc
                .IncreaseTime(fun _ -> 
                    let newVal = min 120 (Timer.timerMinutesVar.Value + 1)
                    Timer.setMinutes(newVal)
                )
                .DecreaseTime(fun _ -> 
                    let newVal = max 1 (Timer.timerMinutesVar.Value - 1)
                    Timer.setMinutes(newVal)
                )
                .ResetTimer(fun _ -> Timer.resetTimer())
                .Doc()
            
      [<SPAEntryPoint>]
    let Main () =
        let renderInnerPage (currentPage: Var<EndPoint>) =
            currentPage.View.Map (fun endpoint ->
                match endpoint with
                | Home        -> Pages.HomePage()
                | Create      -> Pages.CreatePage().Doc()
                | Open id     -> Pages.OpenPage(id).Doc()
                | Timer       -> Pages.TimerPage()
            )
            |> Doc.EmbedView

        IndexTemplate()
            .Content(renderInnerPage currentPage)
            .SwitchToHome(fun _ ->
                currentPage.Value <- Home
            )
            .Bind()
