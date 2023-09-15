library(shiny)
library(flexdashboard)
library(plotly)
library(shinyFiles)

fluidPage(
  titlePanel(tags$b("seq*sample")),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(".main-panel {height: 800px; overflow-y: auto;}")), # Sets the height

      tags$style(HTML("
    .col-sm-4 {
      width: 25%;
    }
    .col-sm-8 {
      width: 75%;
    }
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
      padding: 10px 15px;
      #margin-left: 10px;
  }
  #absoluteBtn {
      background-color: #fff2cc;
      font-size: 9px;
      padding: 10px 15px;
      #margin-left: 10px;
  }
  #relativeBtn {
      background-color: #cce6ff;
      font-size: 9px;
      padding: 10px 15px;
      #margin-right: 10px;
  }
    #folderInput {
      background-color: #a3ffed;
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
