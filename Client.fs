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

    module Pages =
        open WebSharper.UI.Html

        module Home =
            let renderNotes () =
                Notes.View.DocSeqCached(fun note ->
                    IndexTemplate.NoteItem()
                        .NoteTitle(note.Title)
                        .NoteContent(
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
                .TimerProgressBar(progressBarDoc :> Doc)  // Explicitly cast to Doc
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