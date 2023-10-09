#' Function to extract fasta files from Symportal
#'
#' @param folder location of the root Symportal output
#' @param type one of "premed" or "postmed"
#' @export
#' @return its_profile A data.frame of ITS profiles for each sample.ID

extract_fasta <- function(folder, type = "postmed", keep_samples=NULL, drop_samples=NULL){

  if (type == "postmed") {
    file_list <- list.files(path = paste0(folder,"/post_med_seqs/"), pattern = "seqs.fasta", include.dirs = TRUE, recursive = TRUE)
    sym_fasta <- ape::read.dna(paste0(folder, "/post_med_seqs/", file_list), format = "fasta")

  } else if (type == "premed") {
    sym_fasta <- ape::read.dna(paste0(folder, "/pre_med_seqs/pre_med_master_seqs.fasta"), format = "fasta")
  }

  if (!is.null(keep_samples)){
    sym_fasta <- sym_fasta[(names(sym_fasta) %in% keep_samples)]
  }
  if (!is.null(drop_samples)){
    sym_fasta <- sym_fasta[!(names(sym_fasta) %in% keep_samples)]
  }

  return(sym_fasta)

}
