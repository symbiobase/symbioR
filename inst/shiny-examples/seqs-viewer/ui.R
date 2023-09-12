library(shiny)
library(flexdashboard)
library(plotly)

fluidPage(
  titlePanel(tags$b("seq*sample")),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(".main-panel {height: 800px; overflow-y: auto;}")), # Sets the height

      tags$style(HTML("
        .navbar-header .navbar-brand {
          font-size: 36px;
        .sidebar {
          background-color: #f7f7f7;
          padding: 20px;
          border-right: 1px solid #e1e1e1;
        }
        .shiny-input-container {
          margin-bottom: 40px;
        }
        label {
          font-size: 20px;
          font-weight: bold;
          color: #555;
          border-bottom: 1px solid #e1e1e1;
        }
        #absoluteBtn {
            background-color: red;
            padding: 10px 15px;
            margin-left: 10px;
        }
        #relativeBtn {
            background-color: blue;
            padding: 10px 15px;
            margin-right: 10px;
        }
        .shiny-notification {
          font-size: 30px;
        }
      ")),

      #-------------------- Absolute or relative --------------------------@
      div(
        actionButton("relativeBtn", "Relative"),
        actionButton("absoluteBtn", "Absolute"),
        style = "display: inline-block;"
      ),
      #-------------------- Specify folder --------------------------@
      tags$h3("Select folder"),
      textInput("folderInput", "Specify base Symportal folder:", value = "/Users/rof011/symbiodinium/20220919T102058_esampayo"),

      #-------------------- Set threshold --------------------------@
      tags$h3("Set threshold"),
      numericInput("minAbundance", "Lower cutoff for seq abundance:", value = 0, min = 0),
      div(
        style = "display: inline-block; width: 40%;",
        numericInput("minGrey", "Lower cutoff for Grey Switch:", value = 0, min = 0),
      ),
      div(
        style = "display: inline-block; width: 40%;",
        actionButton("toggleGrey", "Grey Switch")
      ),
      #-------------------- SeqID filter --------------------------@
      tags$h3("Filter by seqID"),
      uiOutput("seqID_ui"),
      textInput("seqIDPattern", "Include seq.IDs (comma separated):", value = ""),
      #-------------------- SampleID --------------------------@
      tags$h3("Filter by Sample"),
      uiOutput("sampleID_ui"),
      textInput("excludeSampleIDPatterns", "Exclude sample.IDs (comma seperated)", value = ""),
      #-------------------- Save Functions --------------------------@
      tags$h3("Save Plot"),
      textInput("filenameInput", "Filename:", value = "my_plot.png"),
      div(
        style = "display: inline-block; width: 40%;",
        textInput("plotWidth", "Width:", value = "10")
      ),
      div(
        style = "display: inline-block; width: 40%;",
        textInput("plotHeight", "Height:", value = "6")
      ),
      textInput("saveFolderInput", "Set folder path:", value = "/Users/rof011/symportalfunctions"),
      actionButton("savePlotBtn", "Save Plot")
    ),
    #-------------------- plotly --------------------------@
    mainPanel(
      plotlyOutput("plotUI", height = "95vh")
    )
  )
)
