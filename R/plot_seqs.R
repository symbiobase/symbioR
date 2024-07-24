#' plot_seqs
#'
#' quick visualisation of sequences clustered by sample name either with ggplot or plotly with
#' options to organise samples by outputs of cluster analysis for sample dissimilarity, organise
#' sequences by relative abundance, and set the number of columns and rows for large sample sizes
#'
#' @param input input file of sequences and samples (e.g. from extract_seqs)
#' @param type standard plots ("ggplot") or interactive plots ("plotly")
#' @param cluster optional clustering of sample names by dissimilarity index, one of "bray-curtis", "euclidean", "jaccard", "hellinger", or default ("none")
#' @param facet TRUE/FALSE to facet output, null by default
#' @param ncol number of rows to facet
#' @param seq.order TRUE/FALSE, organise stacked bars by dominant sequences (abundances)
#' @export
#' @return A data.frame of seq_ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.


plot_seqs <- function(input, folder, cols, type = "ggplot", cluster = "none", nrow=NULL, facet = NULL, seq.order=TRUE, ...) {

  #folder=folder_path

  ### update order by dissimilarity index

  if (cluster == "bray-curtis") {
    dist_data <- input |>
      dplyr::select(sample_name, seq_ID, abundance) |>
      tidyr::pivot_wider(names_from = "seq_ID", values_from = "abundance") |>
      tibble::column_to_rownames("sample_name") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), 0, .)))

    hclust_bray <- hclust(vegan::vegdist(vegan::decostand(dist_data, "total"), "bray"))
    updated_order_bray <- hclust_bray$order
    hclust_bray_order <- hclust_bray$labels[updated_order_bray]

    input <- input %>%
      dplyr::mutate(sample_name = factor(sample_name, levels = hclust_bray_order)) %>%
      dplyr::arrange(sample_name)
  }

  if (cluster == "euclidean") {
    dist_data <- input |>
      dplyr::select(sample_name, seq_ID, abundance) |>
      tidyr::pivot_wider(names_from = "seq_ID", values_from = "abundance") |>
      tibble::column_to_rownames("sample_name") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), 0, .)))

    hclust_bray <- hclust(vegan::vegdist(vegan::decostand(dist_data, "total"), "euclidean"))
    updated_order_bray <- hclust_bray$order
    hclust_bray_order <- hclust_bray$labels[updated_order_bray]

    input <- input %>%
      dplyr::mutate(sample_name = factor(sample_name, levels = hclust_bray_order)) %>%
      dplyr::arrange(sample_name)
  }


  if (cluster == "jaccard") {
    dist_data <- input |>
      dplyr::select(sample_name, seq_ID, abundance) |>
      tidyr::pivot_wider(names_from = "seq_ID", values_from = "abundance") |>
      tibble::column_to_rownames("sample_name") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), 0, .)))

    hclust_bray <- hclust(vegan::vegdist(vegan::decostand(dist_data, "total"), "jaccard"))
    updated_order_bray <- hclust_bray$order
    hclust_bray_order <- hclust_bray$labels[updated_order_bray]

    input <- input %>%
      dplyr::mutate(sample_name = factor(sample_name, levels = hclust_bray_order)) %>%
      dplyr::arrange(sample_name)
  }


  if (cluster == "hellinger") {
    dist_data <- input |>
      dplyr::select(sample_name, seq_ID, abundance) |>
      tidyr::pivot_wider(names_from = "seq_ID", values_from = "abundance") |>
      tibble::column_to_rownames("sample_name") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), 0, .)))

    hclust_bray <- hclust(vegan::vegdist(vegan::decostand(dist_data, "total"), "hellinger"))
    updated_order_bray <- hclust_bray$order
    hclust_bray_order <- hclust_bray$labels[updated_order_bray]

    input <- input %>%
      dplyr::mutate(sample_name = factor(sample_name, levels = hclust_bray_order)) %>%
      dplyr::arrange(sample_name)
  }


  if (cluster == "none") {
    input <- input
  }

  # get colors
  colour.seqs_new <- extract_plot_colors(folder)
  #filtered_color_list <- color_list[names(color_list) %in% input$seq_ID]

  dominant.seqs <-  input %>%
    group_by(seq_ID) %>%
    summarize(max_abundance = max(abundance)) %>%
    arrange(desc(max_abundance)) %>%
    pull(seq_ID)

  if(seq.order==TRUE){
    p <-
      ggplot2::ggplot(
        data = input,
        ggplot2::aes(x = sample_name, y = abundance, fill = factor(seq_ID, levels=dominant.seqs), group = factor(seq_ID, levels=dominant.seqs))
             ) +
      ggplot2::scale_y_reverse()


  }

  else if(seq.order==FALSE) {
  p <-
    ggplot2::ggplot(
      data = input,
      ggplot2::aes(x = sample_name, y = abundance, fill = seq_ID, group = abundance)
    )
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity") +
    ggplot2::scale_fill_manual(values = colour.seqs_new) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::theme(legend.position = "none")
#
#
#   if (!is.null(order)) {
#
#     p <-
#       ggplot2::ggplot(
#         data = input,
#         ggplot2::aes(x =  reorder(sample_name, {{order}}), y = abundance, fill = seq_ID, group = abundance)
#       ) +
#       ggplot2::theme_bw() +
#       ggplot2::geom_bar(color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity") +
#       ggplot2::scale_fill_manual(values = colour.seqs_new) +
#       ggplot2::xlab("") +
#       ggplot2::ylab("") +
#       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
#
#
#   }
#


  #--------------------------------------------#
  if (!is.null(nrow)) {

    input <- input

    create_facet_column <- function(df, n) {
      df %>%
        dplyr::group_by(sample_name) %>%
        dplyr::summarise() %>%
        dplyr::mutate(rn = dplyr::row_number()) %>%
        #select(-facet_column) %>%
        dplyr::mutate(facet_column = letters[ceiling(rn / (nrow(.) / n))]) %>%
        dplyr::right_join(df, by = "sample_name")
    }

    input <- create_facet_column(input, nrow)

      if(seq.order==TRUE){
        p <-
          ggplot2::ggplot(
            data = input,
            ggplot2::aes(x = sample_name, y = abundance, fill = factor(seq_ID, levels=dominant.seqs), group = factor(seq_ID, levels=dominant.seqs))
          ) +
          ggplot2::scale_y_reverse() +
          ggplot2::theme(legend.position = "none")


      }

    else if(seq.order==FALSE) {
      p <-
        ggplot2::ggplot(
          data = input,
          ggplot2::aes(x = sample_name, y = abundance, fill = seq_ID, group = abundance)
        )
    }

    p <- p +
      ggplot2::theme_bw() +
      ggplot2::geom_bar(color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity") +
      ggplot2::scale_fill_manual(values = colour.seqs_new) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::facet_wrap(~ facet_column, nrow=nrow, scales = "free_x") +
      ggplot2::theme(panel.spacing = unit(2, "cm", data = NULL)) +
      ggplot2::theme(legend.position = "none")
    }


  #--------------------------------------------#
  if (!is.null(facet)) {
    p <- p +
      ggplot2::facet_wrap(~ get(facet), scales = "free_x") + ggplot2::xlab("") + ggplot2::theme(legend.position = "none")
  }
  #--------------------------------------------#


  if (type == "ggplot") {

    return(p)

  } else if (type == "plotly") {

    p <- plotly::ggplotly(p) %>% plotly::layout(height = p$plotHeight, autosize=TRUE, margin = list(l = 100, r = 100))

    return(p)

  } else {
    print("either one of ggplot or plotly")
  }
}
