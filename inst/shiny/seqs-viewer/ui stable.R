library(shiny)
library(flexdashboard)
library(plotly)
library(shinyFiles)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(".main-panel {height: 1500px; overflow-y: scroll;}")), # Sets the height

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
    color: #125960;
    font-size: 22px;
    font-weight: bold;
    margin-top: 1px;
    margin-bottom: 4px;

  }
  h3 {
    font-size: 14px;
    font-weight: bold;
    margin-top: 6px;
    margin-bottom: 9px;
  }
  label {
    display: none
  }
  p {
    font-size: 11px;
    margin-bottom: 6px;
    padding-bottom: 0;
  }

  .sidebar {
    background-color: #ffffff;
    padding: 0px;
    width: 300px !important;
    max-width: 300px;
    border-right: 1px solid #e1e1e1;
  }
  .sidebox {
    border: 1px solid darkgrey;
    background-color: #f5fcfc;
    border-radius: 6px;
    padding-bottom: 6px;
    padding-left: 6px;
    padding-right: 6px;
  }
  .well {
    background-color: #FFFFFF;
    border-color: #FFFFFF;
  }
 .sidebox-break {
    padding: 5px;
  }
  .shiny-input-container {
    background-color: #ffffff;
    padding-top: 0;
    margin-top: 0;
    margin-bottom: 4px;
  }

 /*--------- buttons -----------*/

  #relativeBtn {
      background-color: #E5D4C3;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 5px 15px;
  }
 #absoluteBtn {
      background-color: #CBC3E5;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 5px 15px;
  }
  #toggleGrey {
      background-color: #888b8c;
      color: #f4f4f4;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 7px 20px;
    }
    #savePlotBtn {
      background-color: #37a09d;
      color: #e6f3f4;
      border: 1px solid black;
      font-size: 11px;
      padding: 10px 25px;
    }
    #saveDataBtn {
      background-color: #216a9b;
      color: #e6f3f4;
      border: 1px solid black;
      font-size: 11px;
      padding: 10px 15px;
  }
    #folderInput {
      background-color: #F2F0CE;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 5px 15px;
      margin-right: 10px;
    }
  #toggleFacetBtn {
      background-color: #EDD6D6;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 5px 25px;
      #margin-left: 10px;
  }
   #toggleclusterBC {
      background-color: #C4E3E8;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 8px;
      #margin-left: 4px;
   }
   #toggleclusterEU {
      background-color: #C3D2E5;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 8px;
      #margin-left: 4px;
   }
   #toggleclusterJC {
      background-color: #C6E5C3;
      border: 1px solid darkgrey;
      font-size: 11px;
      padding: 8px 12px;
      #margin-left: 4px;
  }
  .shiny-notification {
    font-size: 20px;
  }
")),
#-------------------- sidebar --------------------------@
      # div(class="sidebox",
      #     tags$h2("seq*sample viewer"),
      # ),
      # div(class="sidebox-break",
      # ),
      #-------------------- Specify folder --------------------------@
      div(class="sidebox",
          tags$h3("Select folder:"),
          tags$p("Select main (root) symportal folder:"),
          shinyDirButton("folderInput", "Directory", "Please select a directory", FALSE),
      ),
      div(class="sidebox-break",
      ),
      #-------------------- Absolute or relative --------------------------@
      div(class="sidebox",
        tags$h3("Select type:"),
        tags$p("Select relative abundance or absolute abundance for plotting bar graph:"),
        div(
          actionButton("relativeBtn", "Relative"),
          actionButton("absoluteBtn", "Absolute"),
          style = "display: inline-block;"
        ),
      ),
      div(class="sidebox-break",
      ),
      #-------------------- Set threshold --------------------------@
      div(class="sidebox",
        tags$h3("Set threshold:"),
        tags$p("Remove sequences with less than this number of sequence reads:"),
        numericInput("minAbundance", "", value = 0, min = 0),
        tags$p(""),
        tags$p("Convert to greyscale seqs contributing less than (%) to each sample:"),
          div(
          style = "display: inline-block; width: 45%; ",
          numericInput("minGrey", "", value = 0, min = 0),
        ),
        div(
          style = "display: inline-block; width: 45%;",
          actionButton("toggleGrey", "Grey Switch")
        ),
      ),
      div(class="sidebox-break",
      ),
      #-------------------- SeqID / SampleID filter --------------------------@
      div(class="sidebox",
        tags$h3("Filter by seqID:"),
        uiOutput("seqID_ui"),
        tags$p("Include seq.IDs (comma separated):"),
        textInput("seqIDPattern", "", value = ""),
        tags$h3("Filter by Sample:"),
        uiOutput("sampleID_ui"),
        tags$p("Exclude sample.IDs (comma seperated)"),
        textInput("excludeSampleIDPatterns", "", value = ""),
      ),
      div(class="sidebox-break",
      ),
      #-------------------- Facet --------------------------@
      div(class="sidebox",
          tags$h3("Facet"),
          tags$p("Wrap the number of rows for large datasets"),

          div(
            style = "display: inline-block; width: 25%;",
            numericInput("numInput", "Enter a number:", value = 1, min = 1),
          ),

          div(
            style = "display: inline-block; width: 40%;",
            tags$p(" rows"),
          ),

          tags$br(),

          div(
            style = "display: inline-block; width: 40%;",
            numericInput("numInputHeight", "Enter a number:", value = 500, min = 100, max=1200, step=50),
          ),

          div(
            style = "display: inline-block; width: 40%;",
            tags$p(" row height"),
          ),

      ),
      div(class="sidebox-break",
      ),


      #-------------------- type --------------------------@
      div(class="sidebox",
        tags$h3("Cluster samples:"),
        tags$p("Add groupings for samples based on cluster analysis of seqID per sample"),
        ui <- fluidPage(
          selectInput("orderType", "Sample clustering:", c("normal", "Bray-Curtis", "Euclidean", "Jaccard", "Hellingers")),
          tableOutput("tableData")
        )
      ),
      div(class="sidebox-break",
      ),
      #-------------------- Save Functions --------------------------@
      div(class="sidebox",
        tags$h3("Save Plot to folder:"),
#        tags$p("Save plot output to a folder. "),
        tags$p("Filename (either .png, .jpg, .pdf)"),
        textInput("filenameInput", "Filename:", value = "my_plot.png"),
        tags$p("Set width and height:"),
        div(
          style = "display: inline-block; width: 40%;",
          textInput("plotWidth", "Width:", value = "10")
        ),
        div(
          style = "display: inline-block; width: 40%;",
          textInput("plotHeight", "Height:", value = "6")
        ),
        tags$p("Set output folder:"),
        textInput("saveFolderInput", "Set folder path:", value = "/Users/rof011/symbioR"),
        tags$p("Save file or plot data (csv)"),
        actionButton("savePlotBtn", "Save Plot"),
        actionButton("saveDataBtn", "Save Raw Data")
      ),
    ),

    #-------------------- plotly --------------------------@
    mainPanel(
      plotlyOutput("plotUI", height = "95vh")
    )
  )
)
