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
        | [<EndPoint "/tags">] TagsPage
    
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
        CreatedDate: DateTime
        IsFavorite: bool
    }    
    let Notes = ListModel.Create (fun note -> note.Id) []
    let NextId = Var.Create 1
    
    // Add a sample note for testing
    do 
        Notes.Add({
            Id = 0
            Title = "Sample Note"
            Content = "This is a sample note to test the favorite functionality."
            Subject = "Test"
            Tags = []
            CreatedDate = DateTime.Now
            IsFavorite = false
        })

    let router = Router.Infer<EndPoint>()
    let currentPage = Router.InstallHash Home router

    type Router<'T when 'T: equality> with
        member this.LinkHash (ep: 'T) = "#" + this.Link ep

    module Pages =
        open WebSharper.UI.Html        
        let toggleNoteFavorite (noteId: int) =
            Console.Log("Toggle favorite for note:", noteId)
            match Notes.TryFindByKey(noteId) with
            | Some note ->
                let updatedNote = { note with IsFavorite = not note.IsFavorite }
                Notes.RemoveByKey(note.Id)
                Notes.Add(updatedNote)
                Console.Log("Note favorite toggled. New state:", updatedNote.IsFavorite)
                JS.Alert(sprintf "Note %s %s" note.Title (if updatedNote.IsFavorite then "marked as favorite" else "removed from favorites"))
            | None -> 
                Console.Log("Note not found with ID:", noteId)

        module TagsManager =
            let tagTitleVar = Var.Create ""
            let tagColorVar = Var.Create "#6c5ce7"

            let colorOptions = [
                "#6c5ce7", "Purple"
                "#00cec9", "Teal" 
                "#fd79a8", "Pink"
                "#e17055", "Orange"
                "#00b894", "Green"
                "#0984e3", "Blue"
                "#636e72", "Gray"
                "#2d3436", "Dark"
            ]

            let addNewTag() =
                let title = tagTitleVar.Value.Trim()
                let color = tagColorVar.Value
                
                if not (String.IsNullOrWhiteSpace(title)) then
                    let newTag = {
                        Id = NextTagId.Value
                        Title = title
                        Color = color
                    }
                    
                    Tags.Add(newTag)
                    NextTagId.Value <- NextTagId.Value + 1
                    
                    // Reset input fields
                    tagTitleVar.Value <- ""
                    JS.Alert("Tag created successfully!")
                else
                    JS.Alert("Please enter a tag name.")

            let deleteTag tagId =
                Tags.RemoveByKey(tagId)

            let renderTags() =
                Tags.View.DocSeqCached(fun tag ->
                    div [attr.``class`` "tag-item"] [
                        div [attr.``class`` "color-preview"; 
                             Attr.Style "background-color" tag.Color; 
                             Attr.Style "width" "20px"; 
                             Attr.Style "height" "20px"; 
                             Attr.Style "border-radius" "50%"] []
                        span [attr.``class`` "tag-name"] [text tag.Title]
                        button [attr.``class`` "tag-delete-btn"; 
                                attr.``type`` "button";
                                on.click (fun _ _ -> deleteTag tag.Id)] [
                            i [attr.``class`` "fas fa-times"] []
                        ]
                    ]
                )

            let TagsPage() =
                IndexTemplate.TagsPage()
                    .TagName(tagTitleVar : Var<string>)
                    .TagColor(tagColorVar : Var<string>)
                    .TagsList(renderTags())
                    .ColorOptions(
                        Doc.Concat [
                            for color, name in colorOptions ->
                                let id = "color-" + name.ToLower()
                                label [attr.``class`` "color-option";
                                        attr.``for`` id] [
                                    input [attr.``type`` "radio";
                                        attr.``id`` id;
                                        attr.name "tag-color";
                                        attr.value color;
                                        on.change (fun _ _ -> tagColorVar.Value <- color)] []
                                    span [attr.``class`` "color-preview";
                                        Attr.Style "background-color" color] []
                                    text name
                                ]
                        ]
                    )
                    .CreateTag(fun _ -> addNewTag())
                    .Doc()

        module Home =
            let htmlToPlainText (html: string) =
                let tempDiv = JS.Document.CreateElement("div")
                tempDiv.InnerHTML <- html
                
                let plainText = tempDiv.TextContent
                
                if isNull plainText then "" else plainText

            let formatDate (date: DateTime) =
                let months = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]
                sprintf "%s %d, %d" months.[date.Month - 1] date.Day date.Year

            let filterVar = Var.Create "All Notes"
            let searchTermVar = Var.Create ""

            let renderNotes () =
                let filteredNotes = 
                    Notes.View.Map(fun notes ->
                        let initialFiltered =
                            match filterVar.Value with
                            | "Favorites" -> notes |> Seq.filter (fun n -> n.IsFavorite)
                            | "Recent" -> 
                                notes 
                                |> Seq.sortByDescending (fun n -> n.CreatedDate)
                                |> Seq.truncate 5
                            | _ -> notes

                        let searchTerm = searchTermVar.Value.Trim().ToLower()
                        if String.IsNullOrWhiteSpace(searchTerm) then
                            initialFiltered
                        else
                            initialFiltered |> Seq.filter (fun n ->
                                let title = n.Title.ToLower()
                                let content = (htmlToPlainText n.Content).ToLower()
                                title.Contains(searchTerm) || content.Contains(searchTerm)
                            )
                    )

                filteredNotes.DocSeqCached(fun note ->
                    IndexTemplate.NoteItem()
                        .NoteTitle(note.Title)
                        .NoteDate(formatDate note.CreatedDate)
                        .NoteContent(
                            let plainTextContent = htmlToPlainText note.Content
                            if plainTextContent.Length > 300 then 
                                plainTextContent.Substring(0, 100) + "..." 
                            else 
                                plainTextContent)
                        .NoteTags(
                            if note.Tags.IsEmpty then
                                Doc.Empty
                            else
                                Doc.Concat [
                                    for tag in note.Tags ->
                                        span [attr.``class`` "note-tag"
                                              Attr.Style "background-color" tag.Color] [
                                            text tag.Title
                                        ]
                                ]
                        )                        // Ensure proper icon classes are used
                        .FavoriteIcon(
                            if note.IsFavorite then
                                "fas fa-star fa-lg"  // Filled star with larger size
                            else
                                "far fa-star fa-lg"  // Outline star with larger size
                        )
                        .ToggleFavorite(fun _ -> 
                            toggleNoteFavorite note.Id)
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
            let selectedTagsVar = Var.Create (Set.empty<int>)
            let toggleTag tagId =
                let currentTags = selectedTagsVar.Value
                let newTags =
                    if Set.contains tagId currentTags then
                        Set.remove tagId currentTags
                    else
                        Set.add tagId currentTags
                selectedTagsVar.Value <- newTags

            let renderSelectableTags() =
                Tags.View.DocSeqCached(fun (tag: Tag) ->
                    let isSelected = 
                        selectedTagsVar.View 
                        |> View.Map (fun selectedIds -> Set.contains tag.Id selectedIds)
                    
                    div [attr.``class`` "selectable-tag"
                         Attr.DynamicClassPred "selected" isSelected
                         Attr.DynamicStyle "background-color" (
                            isSelected |> View.Map (fun selected ->
                                if selected then tag.Color else "transparent"))
                         Attr.DynamicStyle "color" (
                            isSelected |> View.Map (fun selected ->
                                if selected then "white" else "inherit"))  
                         on.click (fun _ _ -> toggleTag tag.Id)] [
                        text tag.Title
                    ]
                )

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
                    let selectedTags = 
                        selectedTagsVar.Value
                        |> Set.toList
                        |> List.choose (fun tagId -> Tags.TryFindByKey tagId)
                    
                    
                    let newNote = {
                        Id = NextId.Value
                        Title = title
                        Content = content
                        Subject = "General"
                        Tags = selectedTags
                        CreatedDate = DateTime.Now
                        IsFavorite = false
                    }
                    
                    Notes.Add(newNote)
                    
                    NextId.Value <- NextId.Value + 1
                    
                    titleVar.Value <- ""
                    contentVar.Value <- ""
                    selectedTagsVar.Value <- Set.empty
                    contentElement.InnerHTML <- ""
                    
                    currentPage.Value <- Home
                    
                    JS.Alert("Note added successfully!")
                else
                    JS.Alert("Please fill out both title and content fields.")

        let HomePage() =
            IndexTemplate.HomePage()
                .NotesContainer(Home.renderNotes())
                .FilterNotes(fun e ->
                    let select = e.Target :?> HTMLElement
                    let selectedValue = select.GetAttribute("value")
                    // Handle null case
                    let value = if isNull selectedValue then "All Notes" else selectedValue
                    Home.filterVar.Value <- value
                )
                .CurrentFilter(Home.filterVar)
                .SearchNotes(fun e ->
                    let input = e.Target :?> HTMLInputElement
                    Home.searchTermVar.Value <- input.Value
                )
                .SearchTerm(Home.searchTermVar)
                .CreateNewNote(fun _ -> 
                    currentPage.Value <- Create)
                .Doc()
        let CreatePage() =
            let doc = IndexTemplate.CreatePage()

            Create.selectedTagsVar.Value <- Set.empty
            
            JS.Window.SetTimeout(fun () -> 
                Create.initRichTextEditor()
                Console.Log("Rich Text Editor initialized on page load")
            , 100) |> ignore
    
            doc.SelectableTags(Create.renderSelectableTags()) |> ignore
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

                let noteTagIds = note.Tags |> List.map (fun tag -> tag.Id) |> Set.ofList
                Create.selectedTagsVar.Value <- noteTagIds

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
                doc.SelectableTags(Create.renderSelectableTags()) |> ignore
                doc.SaveNote(fun _ ->
                    let titleElement = JS.Document.GetElementById("note-title") :?> HTMLInputElement
                    let contentElement = JS.Document.GetElementById("note-content")
                    let title = if isNull titleElement then "" else titleElement.Value
                    let content = if isNull contentElement then "" else contentElement.InnerHTML
                    let selectedTags = 
                            Create.selectedTagsVar.Value
                            |> Set.toList
                            |> List.choose (fun tagId -> Tags.TryFindByKey tagId)
                    
                    let updatedNote = {
                        Id = note.Id
                        Title = title
                        Content = content
                        Subject = note.Subject
                        Tags = selectedTags
                        CreatedDate = note.CreatedDate
                        IsFavorite = note.IsFavorite
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
                | TagsPage    -> Pages.TagsManager.TagsPage()
            )
            |> Doc.EmbedView

        IndexTemplate()
            .Content(renderInnerPage currentPage)
            .SwitchToHome(fun _ ->
                currentPage.Value <- Home
            )
            .Bind()
