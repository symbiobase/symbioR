library(shiny)
library(flexdashboard)
library(plotly)
library(shinyFiles)

fluidPage(
  titlePanel(tags$b("seq*sample")),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(".main-panel {height: 1200px; overflow-y: auto;}")), # Sets the height

      tags$style(HTML("
    .col-sm-4 {
      width: 300px;
    }
    # .col-sm-8 {
    #   width: 60%;
    # }
  .navbar-header .navbar-brand {
    font-size: 12px;
  }
  h2 {
    font-size: 22px;
    font-weight: bold;
  }
  h3 {
    font-size: 14px;
    font-weight: bold;
  }
   label {
    font-size: 11px;
    color: #555;
    #border-bottom: 3px solid #e1e1e1;
    border-top: 3px solid #e1e1e1;

  }
  .sidebar {
    background-color: #e6ffff;
    padding: 20px;
    width: 300px !important;
    max-width: 300px;
    border-right: 1px solid #e1e1e1;
  }
  .shiny-input-container {
    margin-bottom: 40px;
  }
 #toggleFacetBtn {
      background-color: #e3c7ff;
      font-size: 9px;
      padding: 5px 5;
      #margin-left: 10px;
  }
  #absoluteBtn {
      background-color: #fff2cc;
      font-size: 9px;
      padding: 5px 5px;
      #margin-left: 10px;
  }
  #relativeBtn {
      background-color: #cce6ff;
      font-size: 9px;
      padding: 5px 5px;
      #margin-right: 10px;
  }
    #folderInput {
      background-color: #a9a9a9;
      font-size: 9px;
      padding: 10px 15px;
      margin-right: 10px;
  }
  .shiny-notification {
    font-size: 30px;
  }
"))
,

      #-------------------- Absolute or relative --------------------------@
      div(
        actionButton("relativeBtn", "Relative"),
        actionButton("absoluteBtn", "Absolute"),
        actionButton("toggleFacetBtn", "Facets"),
        style = "display: inline-block;"
      ),
      #-------------------- Specify folder --------------------------@
      tags$h3(""),
      shinyDirButton("folderInput", "Directory", "Please select a directory", FALSE),


      #-------------------- Set threshold --------------------------@
div(
  style = "border: 1px solid black; padding: 6px; border-radius: 6px;", # CSS for border and padding


      tags$h3("Set threshold"),
      numericInput("minAbundance", "Lower cutoff for seqs (abundance):", value = 0, min = 0),
      div(
        style = "display: inline-block; width: 40%;",
        numericInput("minGrey", "Lower cutoff for Grey Switch (%):", value = 0, min = 0),
      ),
      div(
        style = "display: inline-block; width: 40%;",
        actionButton("toggleGrey", "Grey Switch")
      ),
),
div(style = "", tags$h4(""), ),

      #-------------------- SeqID / SampleID filter --------------------------@
div(
  style = "border: 1px solid black; padding: 6px; border-radius: 6px;", # CSS for border and padding

      tags$h3("Filter by seqID"),
      uiOutput("seqID_ui"),
      textInput("seqIDPattern", "Include seq.IDs (comma separated):", value = ""),
      tags$h3("Filter by Sample"),
      uiOutput("sampleID_ui"),
      textInput("excludeSampleIDPatterns", "Exclude sample.IDs (comma seperated)", value = ""),
),
div(style = "", tags$h4(""), ),

      #-------------------- Save Functions --------------------------@
div(
  style = "border: 1px solid black; padding: 6px; border-radius: 6px;", # CSS for border and padding

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
),

    #-------------------- plotly --------------------------@
    mainPanel(
      plotlyOutput("plotUI", height = "95vh")
    )
  )
)
