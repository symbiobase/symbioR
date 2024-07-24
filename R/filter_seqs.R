

#' Function to extract sequences from symportal output in long format
#'
#' @param input Input data.frame with seq_ID and sample_name
#' @param folder Optional, folder containing profile information
#' @param type "relative" or "absolute" to specify type of abundance to return
#' @param drop_samples Vector of samples to exclude by name or pattern
#' @param keep_samples Vector of samples to include by name or pattern
#' @param drop_seqs Vector of sequences to exclude by name or pattern
#' @param keep_seqs Vector of sequences to include by name or pattern
#' @param keep_profiles Logical, whether to keep only sequences in matching ITS2 profile
#' @param drop_profiles Logical, whether to exclude sequences in matching ITS2 profile
#' @param random_sample_n Integer, number of random samples to select
#' @param clade Vector of clades to filter sequences by (e.g., clade=C, clade=("C", "D"))
#' @param threshold Numeric, threshold to remove samples with lower total abundance
#' @param seed Optional, seed value for random sampling
#' @param silent Print messages
#' @export
#' @return A data.frame of seq_ID (columns) and sample.ID (rows) with abundance


filter_seqs <- function(input, folder=NULL, type="relative",
                        drop_samples=NULL, keep_samples=NULL,
                        drop_seqs=NULL, keep_seqs=NULL,
                        keep_profiles=FALSE, drop_profiles=FALSE,
                        random_sample_n = NULL, silent=TRUE,
                        clade = NULL, threshold=0, ...){


  # Handle sample name filters
  if (!is.null(drop_samples)) {

    input <- input %>% as.data.frame() %>%
      dplyr::filter(!grepl(paste(drop_samples, collapse = "|"), sample_name))
  }

  if (!is.null(keep_samples)) {
    input <- input %>% as.data.frame() %>%
      dplyr::filter(grepl(paste(keep_samples, collapse = "|"), sample_name))
  }

  # Handle sequence ID filters
  if (!is.null(drop_seqs)) {
    #input <- dplyr::filter(input, !seq_ID %in% drop_seqs)
    input <- input %>% as.data.frame() %>%
      dplyr::filter(!grepl(paste(drop_seqs, collapse = "|"), seq_ID))

    if (!is.null(drop_seqs) & silent==FALSE){
      print(drop_seqs, n=Inf)
    }

  }
  if (!is.null(keep_seqs)) {
    #input <- dplyr::filter(input, seq_ID %in% keep_seqs)
    input <- input %>% as.data.frame() %>%
      dplyr::filter(!grepl(paste(keep_seqs, collapse = "|"), seq_ID))

    if (!is.null(keep_seqs) & silent==FALSE){
      print(keep_seqs, n=Inf)
    }

  }

  #-------- profiles --------#

  # Drop profiles
  if (isTRUE(drop_profiles)) {

    itsprofiles <- extract_its2_profile(folder_path) %>%
      mutate(seqids = str_split(ITS2.profile.1, pattern = "(?<!\\.)[-/]", n = Inf, simplify = FALSE)) |>
      pull(seqids) |>
      unlist() |>
      unique()

    input <- input %>%
      dplyr::filter(!seq_ID %in% itsprofiles)

    if (isTRUE(drop_profiles) & silent==FALSE){
      print(itsprofiles, n=Inf)
    }

  }

  # Keep profiles
  if (isTRUE(keep_profiles)) {

    itsprofiles <- extract_its2_profile(folder_path) %>%
      mutate(seqids = str_split(ITS2.profile.1, pattern = "(?<!\\.)[-/]", n = Inf, simplify = FALSE)) |>
      pull(seqids) |>
      unlist() |>
      unique()

    input <- input %>%
      dplyr::filter(seq_ID %in% itsprofiles)

    if (isTRUE(keep_profiles) & silent==FALSE){
      print(itsprofiles, n=Inf)
    }


  }

  #-------- random_sample_n --------#

  if (!is.null(random_sample_n)) {
    set.seed(seed)
    sample_name_list <- unique(as.character(input$sample_name))
    random_samples <- sample(sample_name_list, random_sample_n)
    random_samples
    set.seed(NULL)

    input <- input %>%
      dplyr::filter(sample_name %in% random_samples) |>
      as.data.frame()
  }

  #-------- clade --------#

  if (!is.null(clade)) {
    input <- input %>%
      dplyr::filter(grepl(clade, seq_ID)) |>
      as.data.frame()
  }

  #-------- threshold --------#

   # threshold is 0 then don't remove anything
   if (!threshold==0) {


    # filter by abundance only works with abundance. Run a precheck to verify this is
    # abundance data and not relative abundance:
  { precheck <- input |>
        dplyr::group_by(sample_name) |>
        dplyr::summarise(total=sum(abundance, na.rm=TRUE))

      if (sum(precheck$total == 1) >= 5) {
        warning("\n
                Filtering failed:\n
                Filtering by an abundance threshold will only work with abundance input data, e.g. extract_seqs(type=\"relative\").\n
                Re-extract with extract_seqs(type=\"absolute\") to filter by a threshold of sequence abundances \n \n")
        stop()
      }
   }

    # make a threshold list of all samples that are less than or equal the threshold
    threshold_list <- input %>%
      dplyr::group_by(sample_name) %>%
      dplyr::summarize(total_abundance = sum(abundance)) %>%
      dplyr::filter(total_abundance <= threshold) |>
      dplyr::arrange(dplyr::desc(total_abundance))

      if (silent==FALSE){
        print(threshold_list, n=Inf)
      }

    # remove samples not in the threshold list:
    input <- input |>
      dplyr::filter(!sample_name %in% threshold_list$sample_name)

  }

  # convert to relative / absolute abundance
  absolute <- input %>%
    droplevels()

  relative <- input %>%
    droplevels() %>%
    dplyr::group_by(sample_name) %>%
    dplyr::mutate(total=sum(abundance, na.rm = TRUE)) %>%
    dplyr::mutate(abundance=abundance/total) %>%
    dplyr::select(-total)


  #return functions:
  if (type == "absolute") {
    return(absolute)
    } else if (type == "relative") {
    return(relative)
  }

}
