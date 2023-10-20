#' Function to plot sequences from symportal output
#'
#'
#'
#'
#' @param folder location of the root Symportal output
#' @param order order by factor level
#' @param format returns either "relative" or "absolute"
#' @param metadata name of metadata file (e.g. "metadata_20230120T102936.csv") within the Symportal output folder
#' @param factors list of columns for the metadata
#' @param ... passes functions
#' @export

plot_postmed_rel <- function(folder, plotly=FALSE, format, metadata, factors, facet=NULL, ...){

  input <- import_postmed_abs(folder, format="long", metadata=metadata, factors=factors) |>
    dplyr::group_by(sample_name, across(all_of(factors))) |>
    dplyr::mutate(total_abundance = sum(abundance)) |>
    dplyr::mutate(relative_abundance = abundance / total_abundance) |>
    dplyr::ungroup()

  ### get colours
  cols <- extract_plot_colors(folder_path) |>
    as.data.frame() |>
    dplyr::rename("fill.cols"=1) |>
    tibble::rownames_to_column("seq.ID")

  fill.cols <- cols$fill.cols
  input <- dplyr::left_join(input, cols, by="seq.ID")

  plot <- ggplot2::ggplot() + ggplot2::theme_bw() +
    ggplot2::geom_bar(data = inputordered, ggplot2::aes(x = sample_name, y = relative_abundance, fill = seq.ID, group = abundance),
                      color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) + ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme(panel.spacing = ggplot2::unit(0.4, "lines"), strip.text = ggplot2::element_text(colour = 'white'), strip.background = ggplot2::element_rect(fill="white", color="white")) +
    ggplot2::scale_fill_manual(values = fill.cols)

  if (is.null(facet)){

    plot <- plot #+ ggplot2::facet_wrap(~across(all_of(facet)))# |> arrange((across(all_of(order))))

  }



  if (isFALSE(plotly)){

  plotly::ggplotly(plot)


  } else {

  plot

  }



}
