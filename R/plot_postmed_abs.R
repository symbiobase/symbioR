#' Function to plot sequences from symportal output
#'
#'
#'
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @export

plot_postmed_abs <- function(folder){

input <- import_postmed_abs(folder, type="long")

### get colours
cols <- extract_plot_colors(folder_path) |>
  as.data.frame() |>
  dplyr::rename("fill.cols"=1) |>
  tibble::rownames_to_column("seq.ID")

fill.cols <- cols$fill.cols
input <- left_join(input, cols, by="seq.ID")

plot <- ggplot() + theme_bw() +
            geom_bar(data = input, aes(x = sample_name, y = abundance, fill = seq.ID, group = abundance),
                     color = "black", linewidth = 0.1, show.legend = FALSE, stat = "identity") +
            scale_y_continuous(expand = c(0, 0)) + xlab("") + ylab("") +
            theme(panel.spacing = unit(0.4, "lines"), strip.text = element_text(colour = 'white'), strip.background = element_rect(fill="white", color="white")) +
            scale_fill_manual(values = fill.cols)


plotly::ggplotly(plot)

}
