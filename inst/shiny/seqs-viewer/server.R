library(symbioR)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(plotly)
library(shiny)
library(shinyFiles)
library(vegan)


server <- function(input, output, session) {


  #-------------------- Initialize reactive values --------------------------@

  data_vals <- reactiveVal(list(plot_data = NULL, colour.seqs = NULL, its2.type.names = NULL))
  greyFilterActivated <- reactiveVal(FALSE) # by default, the grey filter is off
  facetActivated <- reactiveVal(FALSE)
  clusterBCActivated <- reactiveVal(FALSE)
  clusterEUActivated <- reactiveVal(FALSE)
  clusterJCActivated <- reactiveVal(FALSE)
  abundanceType <- reactiveVal("Relative") # default value
  facetType <- reactiveVal("Normal") # default value

  #-------------------- Get shiny volumes --------------------------@


  volumes <- shinyFiles::getVolumes()()

  #-------------------- Folder input --------------------------@

  default_folder <- getOption("default_folder")

  if (!is.null(default_folder)) {
    # If default_folder is provided, use that as the folder location
    folder_path <- default_folder
    # Your code to process the default_folder, similar to what you do in observeEvent
    plot_data_new <- extract_seqs_long(folder_path, type = "absolute")
    plot_data_new2 <- extract_seqs_wide(folder_path, type = "absolute")
    colour.seqs_new <- extract_plot_colors(folder_path)
    its2.type.names_new <- extract_its2_names(folder_path)
    #metadata_new <- extract_metadata(folder_path) %>% dplyr::select("sample.ID", "sample_uid", "host_genus", "host_species", "collection_longitude", "collection_latitude")

    metafile_path <- paste0(folder_path, "/newmeta.csv")
    if (file.exists(metafile_path)) {
      metadata_new <- read.csv(metafile_path)
    } else {

      cat("No additional metadata selected. \n")

    }

    data_vals(list(plot_data = plot_data_new, plot_data_2 = plot_data_new2, colour.seqs = colour.seqs_new, its2.type.names = its2.type.names_new))
  }


  observeEvent(input$folderInput, {
    shinyDirChoose(input, "folderInput", roots = volumes, filetypes = c("", "txt"))
    folder <- shinyFiles::parseDirPath(volumes, input$folderInput)

    # Check if folder has been properly selected
    if (length(folder) > 0) {
      folder_path <- as.character(folder)
      #print(folder_path) # For debugging
      print(unique(filtered_data$sample.ID))

      plot_data_new <- extract_seqs_long(folder_path, type = "absolute")
      plot_data_new2 <- extract_seqs_wide(folder_path, type = "absolute")
      colour.seqs_new <- extract_plot_colors(folder_path)
      its2.type.names_new <- extract_its2_names(folder_path)
      #metadata_new <- extract_metadata(folder_path)


      data_vals(list(plot_data = plot_data_new, plot_data_2 = plot_data_new2, colour.seqs = colour.seqs_new, its2.type.names = its2.type.names_new))
    } else {
      # Display a notification if there's an issue with folder selection
      showNotification("The selected folder must be a valid symportal folder. Please try again.", type = "error")
    }
  })


  #-------------------- plot heights --------------------------@

  output$plotUI <- renderPlotly({
    req(data_reactive()$plot_data)
    p <- reactivePlot()

    ggplotly(p)
  })


  output$seqID_ui <- renderUI({
    selectInput("seqID", "Select seq.ID:",
      choices = c("ALL", base::unique(data_reactive()$plot_data$seq.ID)),
      selected = "ALL", multiple = TRUE
    )
  })
  output$sampleID_ui <- renderUI({
    selectInput("sample.ID", "Select sample.ID:",
      choices = c("ALL", base::unique(data_reactive()$plot_data$sample.ID)),
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



  #-------------------- reactivePlot --------------------------@

  reactivePlot <- reactive({
    req(data_reactive()$plot_data)
    filtered_data <- data_reactive()$plot_data


    #-------------------- filter by seq.ID --------------------------@
    if ("ALL" %in% input$seqID) {
      filtered_data <- data_reactive()$plot_data
    } else {
      filtered_data <- filtered_data %>% filter(seq.ID %in% input$seqID)
    }
    #-------------------- filter by sample.ID --------------------------@
    if ("ALL" %in% input$sample.ID) {
      filtered_data <- filtered_data # No changes
    } else {
      filtered_data <- filtered_data %>% filter(sample.ID %in% input$sample.ID)
    }
    #-------------------- filter by minAbundance --------------------------@
    if (nchar(input$minAbundance) > 0) {
      filtered_data <- filtered_data %>% filter(abundance > input$minAbundance)
    }
    #-------------------- Pattern match for seq.ID --------------------------@
    if (nchar(input$seqIDPattern) > 0) {
      patterns <- unlist(strsplit(input$seqIDPattern, ",\\s*"))
      pattern_matches_list <- purrr::map(patterns, ~ grepl(.x, plot_data$seq.ID))
      combined_matches <- purrr::reduce(pattern_matches_list, `|`)
      pattern_filtered_data <- plot_data %>% filter(combined_matches)
      filtered_data <- dplyr::bind_rows(filtered_data, pattern_filtered_data)
    }
    #-------------------- Excluding based on sample.ID --------------------------@
    if (nchar(input$excludeSampleIDPatterns) > 0) {
      patterns <- unlist(strsplit(input$excludeSampleIDPatterns, ",\\s*"))
      pattern_matches_list <- purrr::map(patterns, ~ grepl(.x, filtered_data$sample.ID))
      combined_matches <- purrr::reduce(pattern_matches_list, `|`)
      filtered_data <- filtered_data %>% filter(!combined_matches)
    }
    #-------------------- reorder factor levels --------------------------@
    filtered_data$seq.ID <- reorder(filtered_data$seq.ID, filtered_data$abundance)
    filtered_data$seq.ID <- factor(filtered_data$seq.ID, levels = rev(levels(filtered_data$seq.ID)))

    #-------------------- add greyFilter  --------------------------@
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


    #-------------------- join metadata  --------------------------@

    if (file.exists(metafile_path)) {
      filtered_data <- left_join(filtered_data, metadata_new, by="sample.ID")
    } else {

    }


    #-------------------- calculate relative abundance for text labels  --------------------------@

    filtered_data <- filtered_data %>%
      group_by(sample.ID) %>%
      mutate(total = sum(abundance),
             proportion = abundance / total) %>%
      ungroup()


    #-------------------- reorder sample.ID with cluster analysis  --------------------------@

    ### Convert seq.ID abundance to relative abundance for vegdist - note this is on abs not relative

    dist_data <- data_reactive()$plot_data |>
      select(sample.ID, seq.ID, abundance) |>
      pivot_wider(names_from = sample.ID, values_from = abundance, values_fill = 0) |>
      column_to_rownames("seq.ID") |>
      t()

    ### update order by dissimilarity index

    if (input$orderType == "Bray-Curtis") {
      hclust_bray <- hclust(vegdist(decostand(dist_data,"total"), "bray"))
      updated_order_bray <- hclust_bray$order
      hclust_bray_order <- hclust_bray$labels[updated_order_bray]

      filtered_data <- filtered_data %>%
        mutate(sample.ID = factor(sample.ID, levels = hclust_bray_order)) %>%
        arrange(sample.ID)

    }
    if (input$orderType == "Euclidean") {

      hclust_euclidean <- hclust(vegdist(decostand(dist_data,"total"), "euclidean"))
      updated_order_euclidean <- hclust_euclidean$order
      hclust_euclidean_order <- hclust_euclidean$labels[updated_order_euclidean]

      filtered_data <- filtered_data %>%
        mutate(sample.ID = factor(sample.ID, levels = hclust_euclidean_order)) %>%
        arrange(sample.ID)
    }
    if (input$orderType == "Jaccard") {

      hclust_jaccard <- hclust(vegdist(decostand(dist_data,"total"), "jaccard"))
      updated_order_jaccard <- hclust_jaccard$order
      hclust_jaccard_order <- hclust_jaccard$labels[updated_order_jaccard]

      filtered_data <- filtered_data %>%
        mutate(sample.ID = factor(sample.ID, levels = hclust_jaccard_order)) %>%
        arrange(sample.ID)
    }
    if (input$orderType == "Hellingers") {

      hclust_hellinger <- hclust(vegdist(decostand(dist_data,"total"), "hellinger"))
      updated_order_hellinger <- hclust_hellinger$order
      hclust_hellinger_order <- hclust_hellinger$labels[updated_order_hellinger]

      filtered_data <- filtered_data %>%
        mutate(sample.ID = factor(sample.ID, levels = hclust_hellinger_order)) %>%
        arrange(sample.ID)

    } else {
    }

    color_palette <- data_reactive()$colour.seqs

    #-------------------- facet panel  --------------------------@


      dynamic_number <- as.numeric(input$numInput)

      create_facet_column <- function(df, n) {
        df %>%
          group_by(sample.ID) %>%
          summarise() %>%
          mutate(rn = row_number()) %>%
          #select(-facet_column) %>%
          mutate(facet_column = letters[ceiling(rn / (nrow(.) / n))]) %>%
          right_join(df, by = "sample.ID")
      }

      filtered_data <- create_facet_column(filtered_data, dynamic_number)

    #-------------------- initialise plot  --------------------------@


    if (nrow(filtered_data) > 0) {

      p <- ggplot(data = filtered_data,
                  aes(x = sample.ID, y = abundance,
                      text = (paste(
                        #"Species: ", host_species,
                        #"Genus: ", host_genus,
#                        "<br>lon/lat: ", paste0("[",round(collection_longitude,1),"] [", round(collection_latitude,1),"]"),
                        #"<br>Latitude: ", round(collection_latitude,2),
                        "<br>Seq.ID:", seq.ID,
                        "<br>Abundance: ", abundance,
                        "<br>Proportion: ", round(proportion,2))),
                      fill = fill_col, group = abundance)) +
        theme_bw()

      if (abundanceType() == "Relative") {
        p <- p + geom_bar(color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity",  position = position_fill(reverse = TRUE)) +
                  #facet_wrap(~facet_column, ncol = 1, scales = "free_x", strip.position="right") +
                  scale_y_reverse(breaks=seq(0,1,0.2), expand = c(0, 0)) + xlab("") + ylab("")
      } else {
        p <- p +
              geom_bar(color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity") +
                  #facet_wrap(~facet_column, ncol = 1, scales = "free_x", strip.position="right") +
                  scale_y_continuous(expand = c(0, 0)) + xlab("") + ylab("")

      }


      #print(head(filtered_data))
       if (input$facetType == "Normal") {
        p <- p +  facet_wrap(~facet_column, nrow = as.numeric(input$numInput), scales = "free_x", strip.position="right") +
           theme(panel.spacing = unit(0.4, "lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                strip.text = element_text(colour = 'white'), strip.background = element_rect(fill="white", color="white"))
       }

       if (input$facetType == "Host Species") {
          p <- p +  facet_wrap(~host_species, nrow = as.numeric(input$numInput), scales = "free_x", strip.position="right") +
            theme(panel.spacing = unit(0.4, "lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  strip.background = element_rect(fill="white", color="white"))
       }

      if (input$facetType == "Host Genus") {
        p <- p +  facet_wrap(~host_genus,nrow = as.numeric(input$numInput), scales = "free_x", strip.position="right") +
          theme(panel.spacing = unit(0.4, "lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                strip.background = element_rect(fill="white", color="white"))
      }

        if (input$facetType == "Location") {
          p <- p +  facet_wrap(~collection_location,  nrow = as.numeric(input$numInput), scales = "free_x", strip.position="right") +
            theme(panel.spacing = unit(0.4, "lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  strip.background = element_rect(fill="white", color="white"))
        }

      color_palette <- data_reactive()$colour.seqs
      color_palette["grey"] <- "grey"

      p <- p + scale_fill_manual(values = color_palette) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    } else {
      p <- ggplot() +
        theme_bw() +
        annotate("text", x = 1, y = 1, label = "Loading data", size = 6, colour = "aquamarine4") +
        theme_void()
    }

    return(p)
  })


  observeEvent(input$toggleFacetBtn, {
    facetActivated(!facetActivated()) # Toggle the state
  })

  ### Event relative
  observeEvent(input$relativeBtn, {
    abundanceType("Relative")
  })

  ### Event absolute
  observeEvent(input$absoluteBtn, {
    abundanceType("Absolute")
  })

  ### Event absolute grey
  observeEvent(input$toggleGrey, {
    greyFilterActivated(!greyFilterActivated()) # toggle the state
  })

  ### Event saveplot
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
    facet_rows <- as.numeric(input$numInput)
    facet_height <- as.numeric(input$numInputHeight)
    req(data_reactive()$plot_data) # Make sure there's data before plotting
    p <- reactivePlot()
    dynamic_number_val <- as.numeric(reactivePlot()$dynamic_number)
    p <- ggplotly(p, width=as.numeric(input$numInputWidth), height=facet_height*facet_rows, tooltip = "text")
    p %>% layout( margin = list(l = 50, r = 50))
  })
}
