
#' Test function to rapidly plot seqs
#'
#'
#' histo_seq
#'
#' @param folder location of the root Symportal output
#' @param n subsample sample_names by n
#' @param seed value for set.seed
#' @export

histo_seq <- function(folder,random=TRUE, n, seed){

tmp <- extract_seqs(folder_path, type="absolute", arrange=FALSE) |>
  filter_seqs(random_sample_n=n+1, seed=2) |> # seed not working
  filter_seqs(clade="C", seed=1) |>
  arrange(seq_ID)


if(isTRUE(random)){
    seq_ID.string <- data.frame(seq_ID=unique(tmp$seq_ID), number=sample(1:length(unique(tmp$seq_ID))))
  } else if(!isTRUE(random)){
    seq_ID.string <- data.frame(seq_ID=unique(tmp$seq_ID), number=(1:length(unique(tmp$seq_ID))))
}

tmp <- left_join(tmp,seq_ID.string, by="seq_ID")

plot <- ggplot() + theme_bw() +
  facet_wrap(~sample_name) +
  geom_bar(data=tmp, aes(number, abundance, fill=seq_ID), color="black", stat="identity", show.legend=FALSE) +
  geom_density(data=tmp, aes(number, abundance),  color="black", show.legend=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

return(plot)

}
