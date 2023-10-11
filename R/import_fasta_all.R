#' Import fasta
#'
#'
#'
#'
#' @param folder location of the root Symportal output
#' @param type returns either "premed" or "postmed"
#' @export



import_fasta_all <- function(folder, type = "postmed"){

  if (type == "postmed") {
    file_list <- list.files(path = paste0(folder,"/post_med_seqs/"), pattern = "seqs.fasta", include.dirs = TRUE, recursive = TRUE)
    sym_fasta <- ape::read.dna(paste0(folder, "/post_med_seqs/", file_list), format = "fasta")

  } else if (type == "premed") {
    sym_fasta <- ape::read.dna(paste0(folder, "/pre_med_seqs/pre_med_master_seqs.fasta"), format = "fasta")
  }


  listvec2df <- function(l){
    n.obs <- sapply(l, length)
    seq.max <- seq_len(max(n.obs))
    mydf <- data.frame(sapply(l, "[", i = seq.max), stringsAsFactors = FALSE)
    return(mydf)
  }

  tmp <- listvec2df(sym_fasta)

  return(tmp)

}

