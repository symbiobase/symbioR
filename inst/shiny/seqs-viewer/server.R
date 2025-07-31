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
  metadata_vals <- reactiveVal(NULL)
  greyFilterActivated <- reactiveVal(FALSE) # by default, the grey filter is off
  facetActivated <- reactiveVal(FALSE)
  clusterBCActivated <- reactiveVal(FALSE)
  clusterEUActivated <- reactiveVal(FALSE)
  clusterJCActivated <- reactiveVal(FALSE)
  cluster_object <- reactiveVal(NULL)
  abundanceType <- reactiveVal("Relative") # default value
  facetType <- reactiveVal("Normal") # default value

  #-------------------- Get shiny volumes --------------------------@


  volumes <- shinyFiles::getVolumes()()
  shinyDirChoose(input, "folderInput", roots = volumes)
  shinyFileChoose(input, "metadataInput", roots = volumes, filetypes = c("xlsx", "csv"))


  #-------------------- Folder input --------------------------@

  default_folder <- getOption("default_folder")

  if (!is.null(default_folder)) {
    # If default_folder is provided, use that as the folder location
    folder_path <- default_folder
    # Your code to process the default_folder, similar to what you do in observeEvent
    plot_data_new <- extract_seqs(folder_path, type = "absolute")
    plot_data_new2 <- extract_seqs(folder_path, type = "absolute", arrange=TRUE) |> # wide formatting
      pivot_wider(names_from="seq_id", values_from="abundance", values_fill=0) |>
      ungroup() |>
      select(-sample_name)
    colour.seqs_new <- extract_plot_colors(folder_path)
    its2.type.names_new <- extract_its2_names(folder_path)
    #metadata_new <- extract_metadata(folder_path) %>% dplyr::select("sample_name", "sample_uid", "host_genus", "host_species", "collection_longitude", "collection_latitude")

    metafile_path <- paste0(folder_path, "/newmeta.csv")
    if (file.exists(metafile_path)) {
      metadata_new <- read.csv(metafile_path)
    } else {

      cat("No additional metadata selected. \n")

    }

    data_vals(list(plot_data = plot_data_new, plot_data_2 = plot_data_new2, colour.seqs = colour.seqs_new, its2.type.names = its2.type.names_new))
  }


  observeEvent(input$folderInput, {
    folder <- shinyFiles::parseDirPath(volumes, input$folderInput)

    # Check if folder has been properly selected
    if (length(folder) > 0) {
      folder_path <- as.character(folder)
      #print(folder_path) # For debugging
      #print(unique(filtered_data$sample_name))

      plot_data_new <- extract_seqs(folder_path, type = "absolute")
      plot_data_new2 <- plot_data_new |> pivot_wider(names_from="seq_id", values_from="abundance", values_fill=0)
      colour.seqs_new <- extract_plot_colors(folder_path)
      its2.type.names_new <- extract_its2_profile(folder_path)
      #metadata_new <- extract_metadata(folder_path)


      data_vals(list(plot_data = plot_data_new, plot_data_2 = plot_data_new2, colour.seqs = colour.seqs_new, its2.type.names = its2.type.names_new))
    } else {
      # Display a notification if there's an issue with folder selection
      showNotification("The selected folder must be a valid symportal folder. Please try again.", type = "error")
    }
  })

  #-------------------- metadata input --------------------------@


  observeEvent(input$metadataInput, {
  #shinyFiles::shinyFileChoose(input, "metadataInput", roots = volumes, filetypes = c("csv", "xlsx"))

  fileinfo <- shinyFiles::parseFilePaths(volumes, input$metadataInput)

  if (nrow(fileinfo) > 0) {
    metafile_path <- as.character(fileinfo$datapath)

    # Read the file and store into reactive
    metadata_new <- if (grepl("\\.xlsx$", metafile_path)) {
      readxl::read_xlsx(metafile_path, skip=1)
    } else {
      read.csv(metafile_path)
    }

    metadata_new <- metadata_new |> filter(sample_name %in% data_vals()$plot_data$sample_name)
    metadata_vals(metadata_new)
  }
})

  #-------------------- plot heights --------------------------@

  output$plotUI <- renderPlotly({
    req(data_reactive()$plot_data)
    p <- reactivePlot()

    ggplotly(p)
  })


  output$seq_id_ui <- renderUI({
    selectInput("seq_id", "Select seq_id:",
      choices = c("ALL", base::unique(data_reactive()$plot_data$seq_id)),
      selected = "ALL", multiple = TRUE
    )
  })
  output$sample_id_ui <- renderUI({
    selectInput("sample_name", "Select sample_name:",
      choices = c("ALL", base::unique(data_reactive()$plot_data$sample_name)),
      selected = "ALL", multiple = TRUE
    )
  })


  observe({
    #print(input$seq_id)
  })


  data_reactive <- reactive({
    req(data_vals()$plot_data)
    data_vals()
  })



  #-------------------- reactivePlot --------------------------@

  reactivePlot <- reactive({
    req(data_reactive()$plot_data)
    filtered_data <- data_reactive()$plot_data


    #-------------------- filter by seq_id --------------------------@
    if ("ALL" %in% input$seq_id) {
      #print(input$seq_id)
      filtered_data <- data_reactive()$plot_data
    } else {
      filtered_data <- filtered_data %>% filter(seq_id %in% input$seq_id)
    }
    #-------------------- filter by sample_name --------------------------@
    if ("ALL" %in% input$sample_name) {
      filtered_data <- filtered_data # No changes
    } else {
      #filtered_data <- filtered_data %>% filter(sample_name %in% input$sample_name)
    }
    #-------------------- filter by minAbundance --------------------------@
    if (nchar(input$minAbundance) > 0) {
      filtered_data <- filtered_data %>% filter(abundance > input$minAbundance)
    }
    #-------------------- Pattern match for seq_id --------------------------@
    # if (nchar(input$seq_idPattern) > 0) {
    #   patterns <- unlist(strsplit(input$seq_idPattern, ",\\s*"))
    #   pattern_matches_list <- purrr::map(patterns, ~ grepl(.x, plot_data$seq_id))
    #   combined_matches <- purrr::reduce(pattern_matches_list, `|`)
    #   pattern_filtered_data <- plot_data %>% filter(combined_matches)
    #   filtered_data <- dplyr::bind_rows(filtered_data, pattern_filtered_data)
    # }
    #-------------------- Excluding based on sample_name --------------------------@
    if (nchar(input$excludeSampleIDPatterns) > 0) {
      patterns <- unlist(strsplit(input$excludeSampleIDPatterns, ",\\s*"))
      pattern_matches_list <- purrr::map(patterns, ~ grepl(.x, filtered_data$sample_name))
      combined_matches <- purrr::reduce(pattern_matches_list, `|`)
      filtered_data <- filtered_data %>% filter(!combined_matches)
    }
    #-------------------- reorder factor levels --------------------------@
    filtered_data$seq_id <- reorder(filtered_data$seq_id, filtered_data$abundance)
    filtered_data$seq_id <- factor(filtered_data$seq_id, levels = rev(levels(filtered_data$seq_id)))

    #-------------------- add greyFilter  --------------------------@
    filtered_data <- filtered_data %>%
      group_by(sample_name) %>%
      arrange(abundance) %>%
      mutate(
        cumulative_abundance = cumsum(abundance),
        total_abundance = sum(abundance),
        cumulative_percentage = cumulative_abundance / total_abundance
      ) %>%
      ungroup() %>%
      mutate(fill_col = ifelse(greyFilterActivated() & cumulative_percentage <= (input$minGrey / 100), "grey", as.character(seq_id)))


    #-------------------- join metadata  --------------------------@

    if (!is.null(metadata_vals())) {
      filtered_data <- dplyr::left_join(filtered_data, metadata_vals(), by = "sample_name")
    } else {
      # No metadata available; skip join
      filtered_data <- filtered_data
    }


    #-------------------- calculate relative abundance for text labels  --------------------------@

    filtered_data <- filtered_data %>%
      group_by(sample_name) %>%
      mutate(total = sum(abundance),
             proportion = abundance / total) %>%
      ungroup()


    #-------------------- reorder sample_name with cluster analysis  --------------------------@

    ### Convert seq_id abundance to relative abundance for vegdist - note this is on abs not relative

    dist_data <- data_reactive()$plot_data |>
      select(sample_name, seq_id, abundance) |>
      pivot_wider(names_from = sample_name, values_from = abundance, values_fill = 0) |>
      column_to_rownames("seq_id") |>
      t()

    ### update order by dissimilarity index

    if (input$orderType == "Bray-Curtis") {
      hclust <- hclust(vegdist(decostand(dist_data,"total"), "bray"))
      cluster_object(hclust)
      updated_order <- hclust$order
      hclust_order <- hclust$labels[updated_order]

      filtered_data <- filtered_data %>%
        mutate(sample_name = factor(sample_name, levels = hclust_order)) %>%
        arrange(sample_name)

    }
    if (input$orderType == "Euclidean") {

      hclust <- hclust(vegdist(decostand(dist_data,"total"), "euclidean"))
      cluster_object(hclust)
      updated_order <- hclust$order
      hclust_order <- hclust$labels[updated_order]

      filtered_data <- filtered_data %>%
        mutate(sample_name = factor(sample_name, levels = hclust_order)) %>%
        arrange(sample_name)
    }
    if (input$orderType == "Jaccard") {

      hclust <- hclust(vegdist(decostand(dist_data,"total"), "jaccard"))
      cluster_object(hclust)
      updated_order <- hclust$order
      hclust_order <- hclust$labels[updated_order]

      filtered_data <- filtered_data %>%
        mutate(sample_name = factor(sample_name, levels = hclust_order)) %>%
        arrange(sample_name)
    }
    if (input$orderType == "Hellingers") {

      hclust <- hclust(vegdist(decostand(dist_data,"total"), "hellinger"))
      cluster_object(hclust)
      updated_order <- hclust$order
      hclust_order <- hclust$labels[updated_order]

      filtered_data <- filtered_data %>%
        mutate(sample_name = factor(sample_name, levels = hclust_order)) %>%
        arrange(sample_name)

    } else {
    }

    color_palette <- data_reactive()$colour.seqs

    #-------------------- facet panel  --------------------------@


      dynamic_number <- as.numeric(input$numInput)

      create_facet_column <- function(df, n) {
        df %>%
          group_by(sample_name) %>%
          summarise() %>%
          mutate(rn = row_number()) %>%
          #select(-facet_column) %>%
          mutate(facet_column = letters[ceiling(rn / (nrow(.) / n))]) %>%
          right_join(df, by = "sample_name")
      }

      filtered_data <- create_facet_column(filtered_data, dynamic_number)

    #-------------------- initialise plot  --------------------------@


    if (nrow(filtered_data) > 0) {

      p <- ggplot(data = filtered_data,
                  aes(x = sample_name, y = abundance,
                      text = (paste(
                        #"Species: ", host_species,
                        #"Genus: ", host_genus,
#                        "<br>lon/lat: ", paste0("[",round(collection_longitude,1),"] [", round(collection_latitude,1),"]"),
                        #"<br>Latitude: ", round(collection_latitude,2),
                        "<br>seq_id:", seq_id,
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

  library(ggdendro)

  output$dendrogramPlot <- renderPlot({
    req(cluster_object())

    hc <- cluster_object()
    dendro <- as.dendrogram(hc)
    dendro_data <- ggdendro::dendro_data(dendro)

    ggplot(ggdendro::segment(dendro_data)) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
      scale_x_continuous(breaks = seq_along(hc$labels), labels = hc$labels, position = "top") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      ) +
      ggtitle(paste("Sample Clustering Dendrogram -", input$orderType))
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
