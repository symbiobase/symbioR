#' Function to extract fasta files from Symportal
#'
#' @param folder location of the root Symportal output
#' @param type one of "premed" or "postmed"
#' @export
#' @return its_profile A data.frame of ITS profiles for each sample.ID

extract_fasta <- function(folder, type = "postmed") {

  if (type == "postmed") {

    file_list <- list.files(path = paste0(folder,"/post_med_seqs/"), pattern = "seqs.fasta", include.dirs = TRUE, recursive = TRUE)
    sym_fasta <- ape::read.dna(paste0(folder, "/post_med_seqs/", file_list), format = "fasta")
    return(sym_fasta)

  } else if (type == "premed") {

      sym_fasta <- ape::read.dna(paste0(folder, "/pre_med_seqs/pre_med_master_seqs.fasta"), format = "fasta")
      return(sym_fasta)

  }
}
