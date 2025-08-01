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
#' @return A data.frame of seq_id (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.

plot_seqs <- function(input, folder, cols, type = "ggplot", cluster = "none", nrow = NULL,
                      facet = NULL, seq.order = TRUE, axis = "normal", sort = NULL, ...) {

  if (cluster != "none") {
    dist_data <- input |>
      dplyr::select(sample_name, seq_id, abundance) |>
      tidyr::pivot_wider(names_from = "seq_id", values_from = "abundance") |>
      tibble::column_to_rownames("sample_name") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), 0, .)))

    distance_method <- switch(cluster,
                              "bray-curtis" = "bray",
                              "euclidean" = "euclidean",
                              "jaccard" = "jaccard",
                              "hellinger" = "hellinger")

    hclust_obj <- hclust(vegan::vegdist(vegan::decostand(dist_data, "total"), distance_method))
    ordered_samples <- hclust_obj$labels[hclust_obj$order]

    input <- input %>%
      dplyr::mutate(sample_name = factor(sample_name, levels = ordered_samples)) %>%
      dplyr::arrange(sample_name)
  }
  # Define colors
  colour.seqs_new <- extract_plot_colors(folder)

  # Sequence order
  dominant.seqs <- input %>%
    dplyr::group_by(seq_id) %>%
    dplyr::summarize(max_abundance = max(abundance)) %>%
    dplyr::arrange(desc(max_abundance)) %>%
    dplyr::pull(seq_id)

  # Build ggplot base
  aes_fill <- if (seq.order) {
    ggplot2::aes(x = sample_name, y = abundance,
                 fill = factor(seq_id, levels = dominant.seqs),
                 group = factor(seq_id, levels = dominant.seqs))
  } else {
    ggplot2::aes(x = sample_name, y = abundance, fill = seq_id, group = abundance)
  }

  p <- ggplot2::ggplot(input, aes_fill) +
    ggplot2::geom_bar(color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity") +
    ggplot2::scale_fill_manual(values = colour.seqs_new) +
    ggplot2::theme_bw() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none"
    )

  #---------------- Axis and Faceting ----------------#

  if (!is.null(facet) && facet %in% names(input)) {
    facet_formula <- as.formula(paste("~", facet))
  } else if (!is.null(facet)) {
    stop(paste("Facet variable", facet, "not found in input"))
  } else {
    facet_formula <- NULL
  }

  # Apply facet and axis direction
  if (axis == "flipped") {
    if (!is.null(facet_formula)) {
      p <- p +
        ggplot2::facet_wrap(facet_formula, nrow = nrow, scales = "free_y") +
        ggplot2::theme(panel.spacing = ggplot2::unit(2, "cm"))
    }
    p <- p + ggplot2::coord_flip()
  } else if (axis == "normal") {
    if (!is.null(facet_formula)) {
      p <- p +
        ggplot2::facet_wrap(facet_formula, nrow = nrow, scales = "free_x") +
        ggplot2::theme(panel.spacing = ggplot2::unit(2, "cm"))
    }
  }

  #---------------- Output ----------------#

  if (type == "ggplot") {
    return(p)
  } else if (type == "plotly") {
    return(plotly::ggplotly(p) %>%
             plotly::layout(height = p$plotHeight, autosize = TRUE,
                            margin = list(l = 100, r = 100)))
  } else {
    stop("type must be 'ggplot' or 'plotly'")
  }

}
