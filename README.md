# SmartNotes - F# Project Omega

## Motivation

SmartNotes was built to provide a clean, functional, and efficient note-taking application that leverages the power of F# and WebSharper for a rich web experience. The application aims to solve common note organization problems by offering a modern UI with features like tags, favorites, and search functionality, all wrapped in a responsive design.

The project demonstrates how functional programming principles can be applied to create interactive web applications with minimal code while maintaining type safety and a clear separation of concerns.

## Features

- **Rich Text Editor**: Format your notes with bold, italic, underline, lists, and text alignment options
- **Tag Management**: Create and organize notes with colorful tags
- **Favorites System**: Mark important notes as favorites for quick access
- **Search & Filter**: Easily find notes using the search function or filter by recency or favorites
- **Responsive Design**: Works on desktop and mobile devices
- **Pomodoro Timer**: Built-in timer feature to help you stay focused while taking notes
- **Real-time Updates**: Changes to notes and tags are reflected immediately in the UI

## Getting Started

### Prerequisites

- [.NET SDK 8.0](https://dotnet.microsoft.com/download) or later
- [Node.js](https://nodejs.org/) (for bundling client assets)

### Installation

1. Clone the repository:
   ```powershell
   git clone https://github.com/BlackWolf222/FSHARP-project-omega.git
   cd FSHARP-project-omega
   ```

2. Restore dependencies:
   ```powershell
   dotnet restore
   npm install
   ```

### Running the Application

**Development Mode:**
```powershell
dotnet run
```

This will start the application with hot reload enabled. Navigate to `https://localhost:58560` in your browser.

**Production Build:**
```powershell
dotnet publish -c Release
```

## Usage

### Creating a Note
1. Click on "New Note" in the sidebar or the "Create Note" button on the home page
2. Enter a title and content using the rich text editor
3. Optionally, select tags to categorize your note
4. Click "Save Note" to save your changes

### Managing Tags
1. Navigate to the "Manage Tags" section from the sidebar
2. Enter a tag name and select a color
3. Click "Create Tag" to add it to your collection
4. Your tags will be available when creating or editing notes

### Using the Timer
1. Click on the "Timer" option in the sidebar
2. Set your desired time using the + and - buttons
3. Click "Start" to begin the countdown
4. When the timer finishes, you'll receive an alert

### Organizing Notes
- Use the search bar to find notes by title or content
- Use the dropdown filter to show all notes, recent notes, or favorites
- Click the star icon on any note to toggle its favorite status

## Project Structure

```
FSHARP-project-omega/
├── Client.fs                 # Client-side code and application logic
├── Startup.fs                # ASP.NET Core startup configuration
├── FSHARP-project-omega.fsproj # Project file with dependencies
├── package.json              # Node.js package configuration
├── esbuild.config.mjs        # ESBuild configuration for bundling
├── vite.config.js            # Vite configuration for development
├── wsconfig.json             # WebSharper configuration
├── wwwroot/                  # Static web assets
│   ├── index.html            # Main HTML template
│   ├── style.css             # CSS styles
│   └── Scripts/              # Compiled JavaScript files
└── Properties/
    └── launchSettings.json   # Development server settings
```

## 🔧 Dependencies

### .NET Dependencies
- **WebSharper (v9.0.1.549)**: F# framework for web development
- **WebSharper.UI (v9.0.0.547)**: UI component library for WebSharper
- **WebSharper.AspNetCore (v9.0.1.549)**: ASP.NET Core integration for WebSharper
- **FSharp.Core (v9.0.202)**: F# core libraries

### Frontend Dependencies
- **Font Awesome (v6.0.0)**: Icon library
- **Google Fonts (Poppins)**: Typography
- **ESBuild (v0.19.9)**: JavaScript bundler
- **ESlint (v9.26.0)**: JavaScript linter

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Any contributions you make are **greatly appreciated**.

### How to Contribute

1. **Fork the Project**: Create your own copy of the repository
2. **Create your Feature Branch**: 
   ```powershell
   git checkout -b feature/AmazingFeature
   ```
3. **Make your Changes**: Implement your feature or bug fix
4. **Run Tests**: Ensure your changes don't break existing functionality
   ```powershell
   dotnet test
   ```
5. **Commit your Changes**:
   ```powershell
   git commit -m 'Add some AmazingFeature'
   ```
6. **Push to the Branch**:
   ```powershell
   git push origin feature/AmazingFeature
   ```
7. **Open a Pull Request**: Go to the repository on GitHub and open a pull request

### Contribution Guidelines

- Follow the existing code style and patterns
- Write clean, well-commented code
- Update documentation for any new features
- Add appropriate tests for new functionality
- Make sure all tests pass before submitting your changes

---

## Author

Created by Beödők Levente. Suggestions and PRs welcome!
