#' Function to plot seq.IDs per sample and hierarchical clustering of sample order with dendrogram
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @param clade filter by single "C" or multiple clades c("C", "D") to filter sequences by
#' @param drop_samples drop samples by named vector, e.g. c("H00B07", "H00B06"), or by one or more partial matches, e.g. c("07","B06")
#' @param keep_samples drop samples by named vector, e.g. c("H00B07", "H00B06"), or by one or more partial matches, e.g. c("07","B06")
#' @param drop_seqs drop seqs by named vector, e.g. c("X2777817_G", "X2777816_G"), or by one or more partial matches, e.g. c("X2","OT")
#' @param threshold Set threshold to remove samples if less than the threshold (defaults to 1000)
#' @param silent defaults to TRUE, if FALSE then prints a list of removed sample names
#' @param ... pass
#' @export
#' @return ggplot and dendrogram for seqs/samples


plot_seqs2 <- function(input, folder, format="ggplot", facet=TRUE){


  df=input

  cols <- symportalfunctions::extract_plot_colors(folder_path) |>
    as.data.frame() |>
    dplyr::rename("col"=1) |>
    tibble::rownames_to_column("seq.ID")

  df2 <- dplyr::left_join(df, cols, by="seq.ID") |> ungroup() |>
    group_by(sample.ID) |>
    arrange(desc(abundance))


  if (isFALSE(facet)){

    p <- ggplot2::ggplot(data = df2,
                            aes(x = reorder(sample.ID, collection_latitude), y = abundance, fill=col, show.legend=TRUE,
                                text = (paste(
                                  "Genera: ", host_genus,
                                  "<br>Latitude: ", round(collection_latitude,2) )))) +
      ggplot2::theme_bw() +
      ggplot2::xlab("") +
      ggplot2::geom_bar(data = df2,
                        color = "black", linewidth = 0.1,
                        show.legend = FALSE, position = position_fill(reverse = TRUE), stat="identity") +
      ggplot2::scale_fill_identity() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  } else {


    p <- ggplot2::ggplot(data = df2,
                            aes(x = reorder(sample.ID, collection_latitude), y = abundance, fill=col, show.legend=TRUE,
                                text = (paste(
                                  "Genera: ", host_genus,
                                  "<br>Latitude: ", round(collection_latitude,2) )))) +
      ggplot2::theme_bw() +
      ggplot2::xlab("") +
      ggplot2::geom_bar(data = df2,
                        color = "black", linewidth = 0.1,
                        show.legend = FALSE, position="stack", stat="identity") +
      ggplot2::scale_fill_identity() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +

      facet_wrap( ~ host_genus, scales="free_x", ncol=3)


  }

  if (format=="plotly"){

  plot <- plotly::ggplotly(p, height=3*250)
  return(plot)

  } else {

    return(p)

  }


}
