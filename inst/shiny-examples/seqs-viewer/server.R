library(symportalfunctions)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(plotly)
library(shiny)
library(shinyFiles)

server <- function(input, output, session) {
  # Initialize reactive values
  data_vals <- reactiveVal(list(plot_data = NULL, colour.seqs = NULL, its2.type.names = NULL))
  greyFilterActivated <- reactiveVal(FALSE) # by default, the grey filter is off


  volumes <- shinyFiles::getVolumes()()

observeEvent(input$folderInput, {

  shinyDirChoose(input, 'folderInput', roots=volumes, filetypes=c('', 'txt'))
  folder <- shinyFiles::parseDirPath(volumes, input$folderInput)
    # Check if folder has been properly selected
    if (length(folder) > 0) {
      folder_path <- as.character(folder)
      print(folder_path) # For debugging

      plot_data_new <- extract_seqs_long(folder_path, type = "absolute")
      colour.seqs_new <- extract_plot_colors(folder_path)
      its2.type.names_new <- extract_its2_names(folder_path)
      data_vals(list(plot_data = plot_data_new, colour.seqs = colour.seqs_new, its2.type.names = its2.type.names_new))

    } else {
      # Display a notification if there's an issue with folder selection
      showNotification("There was an issue with the folder selection. Please try again.", type = "error")
    }
  })





  output$seqID_ui <- renderUI({
    selectInput("seqID", "Select seq.ID:",
      choices = c("ALL", unique(data_reactive()$plot_data$seq.ID)),
      selected = "ALL", multiple = TRUE
    )
  })

  output$seqID_ui <- renderUI({
    selectInput("seqID", "Select seq.ID:",
      choices = c("ALL", unique(data_reactive()$plot_data$seq.ID)),
      selected = "ALL", multiple = TRUE
    )
  })
  output$sampleID_ui <- renderUI({
    selectInput("sample.ID", "Select sample.ID:",
      choices = c("ALL", unique(data_reactive()$plot_data$sample.ID)),
      selected = "ALL", multiple = TRUE
    )
  })


  observe({
    print(input$seqID)
  })

data_reactive <- reactive({
    req(data_vals()$plot_data)
    data_vals()
})


  reactivePlot <- reactive({
    req(data_reactive()$plot_data)
    filtered_data <- data_reactive()$plot_data

    # Your data processing code

    filtered_data <- data_reactive()$plot_data

    if ("ALL" %in% input$seqID) {
      filtered_data <- data_reactive()$plot_data
    } else {
      filtered_data <- filtered_data %>% filter(seq.ID %in% input$seqID)
    }

    if ("ALL" %in% input$sample.ID) {
      filtered_data <- filtered_data  # No changes
    } else {
      filtered_data <- filtered_data %>% filter(sample.ID %in% input$sample.ID)
    }


    if (nchar(input$minAbundance) > 0) {
      filtered_data <- filtered_data %>% filter(abundance > input$minAbundance)
    }

    # Pattern match for seqID
    if (nchar(input$seqIDPattern) > 0) {
      patterns <- unlist(strsplit(input$seqIDPattern, ",\\s*"))
      pattern_matches_list <- purrr::map(patterns, ~ grepl(.x, plot_data$seq.ID))
      combined_matches <- purrr::reduce(pattern_matches_list, `|`)
      pattern_filtered_data <- plot_data %>% filter(combined_matches)
      filtered_data <- dplyr::bind_rows(filtered_data, pattern_filtered_data)
    }

    # Excluding based on sample.ID
    if (nchar(input$excludeSampleIDPatterns) > 0) {
      patterns <- unlist(strsplit(input$excludeSampleIDPatterns, ",\\s*"))
      pattern_matches_list <- purrr::map(patterns, ~ grepl(.x, filtered_data$sample.ID))
      combined_matches <- purrr::reduce(pattern_matches_list, `|`)
      filtered_data <- filtered_data %>% filter(!combined_matches)
    }

    filtered_data$seq.ID <- reorder(filtered_data$seq.ID, filtered_data$abundance)
    filtered_data$seq.ID <- factor(filtered_data$seq.ID, levels = rev(levels(filtered_data$seq.ID)))

    filtered_data <- filtered_data %>%
      group_by(sample.ID) %>%
      arrange(abundance) %>%
      mutate(
        cumulative_abundance = cumsum(abundance),
        total_abundance = sum(abundance),
        cumulative_percentage = cumulative_abundance / total_abundance
      ) %>%
      ungroup() %>%
      mutate(fill_col = ifelse(greyFilterActivated() & cumulative_percentage <= (input$minGrey / 100), "grey", as.character(seq.ID)))

    p <- ggplot() +
      theme_bw()

    if (abundanceType() == "Relative") {
      p <- p + geom_bar(
        data = filtered_data,
        aes(
          x = sample.ID, y = abundance,
          fill = fill_col,
          group = abundance
        ),
        color = "black", linewidth = 0.1, position = position_fill(reverse = TRUE),
        show.legend = FALSE, stat = "identity"
      ) + scale_y_reverse()
    } else {
      p <- p + geom_bar(
        data = filtered_data,
        aes(
          x = sample.ID, y = abundance,
          fill = fill_col,
          group = abundance
        ),
        color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity"
      )
    }

    color_palette <- data_reactive()$colour.seqs
    color_palette["grey"] <- "grey"

    p <- p + scale_fill_manual(values = color_palette) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    return(p)
  })

  abundanceType <- reactiveVal("Relative") # default value

  observeEvent(input$relativeBtn, {
    abundanceType("Relative")
  })

  observeEvent(input$absoluteBtn, {
    abundanceType("Absolute")
  })

  observeEvent(input$toggleGrey, {
    greyFilterActivated(!greyFilterActivated()) # toggle the state
  })

  observeEvent(input$savePlotBtn, {
    folder_path <- input$saveFolderInput
    file_name <- input$filenameInput
    if (substr(folder_path, nchar(folder_path), nchar(folder_path)) != "/") {
      folder_path <- paste0(folder_path, "/")
    }
    full_path <- paste0(folder_path, file_name)
    plotH <- as.numeric(input$plotHeight)
    plotW <- as.numeric(input$plotWidth)
    p <- reactivePlot()
    tryCatch(
      {
        ggsave(filename = full_path, plot = p, width = plotW, height = plotH)
        showNotification("Plot saved successfully!", type = "message")
      },
      error = function(e) {
        showNotification(paste("An error occurred with the following warning:", e$message, "check the file directory and image type (.jpg, .pdf, .png) and width/height are correctly specified"), type = "error")
      }
    )
  })

  output$plotUI <- renderPlotly({
    req(data_reactive()$plot_data) # Make sure there's data before plotting
    p <- reactivePlot()
    ggplotly(p)
  })

}
