#' Function to plot seq.IDs per sample and hierarchical clustering of sample order with dendrogram
#'
#' @param folder location of the root Symportal output
#' @param type one of "relative" or "absolute"
#' @param dist a distance matrix method from vegan, see ?vegdist for details
#' @export
#' @return ggplot and dendrogram for seqs/samples


plot_seqs_dendro <- function(folder, type="relative", dist="bray"){

  df <- symportalfunctions::extract_seqs_long(folder)
  cols <- symportalfunctions::extract_plot_colors(folder_path) |>
    as.data.frame() |>
    dplyr::rename("col"=1) |>
    tibble::rownames_to_column("seq.ID")

  df2 <- dplyr::left_join(df, cols, by="seq.ID")

  dist_data <- df |>
    dplyr::select(sample.ID, seq.ID, abundance) |>
    tidyr::pivot_wider(names_from = sample.ID, values_from = abundance,  values_fill = 0) |>
    tibble::column_to_rownames("seq.ID") |>
    t()

  hclust_dist <- dist_data |>
    vegan::vegdist(dist) |>
    stats::hclust()


  plot <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::xlab("") +
    ggplot2::geom_bar(data = df2,
                     ggplot2::aes(x = sample.ID, y = abundance, fill = col,  group = abundance),
      color = "black", linewidth = 0.1, position = ggplot2::position_fill(reverse = TRUE),
      show.legend = FALSE, stat = "identity") + ggplot2::scale_y_reverse() +
    ggh4x::scale_x_dendrogram(hclust=hclust_dist, position="bottom") +
    ggplot2::scale_fill_identity() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  return(plot)

}
