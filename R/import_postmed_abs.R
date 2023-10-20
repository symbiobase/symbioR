#' Function to extract postmed sequences from symportal output
#'
#'
#'
#'
#' @param folder location of the root Symportal output
#' @param metadata name of metadata file (e.g. "metadata_20230120T102936.csv") within the Symportal output folder
#' @param factors list of columns for the metadata
#' @param type "relative" or "absolute"
#'
#' @export

import_postmed_abs <- function(folder, metadata, factors, format, ...){

  file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)

  full_data <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    dplyr::slice(-dplyr::n())  # remove the last row, summary data

  metadf <- read.csv(paste0(folder, "/", metadata)) |>
    dplyr::select(all_of(factors))

  #metadf$sample_names %in% full_data$sample_name # add STOP here

  full_data <- dplyr::left_join(metadf, full_data, by="sample_name")

  if (format=="long"){

  full_data_long <- full_data %>%
    tidyr::pivot_longer(cols = -all_of(factors), names_to = "seq.ID", values_to = "abundance")

  return(full_data_long)

  } else if (format=="wide"){

  return(full_data)

  } else {

    print("one of [long] or [wide]")
  }

}


# factors <- c("sample_name", "location", "collection_latitude", "host_genus")
#
# metadata_20230120T102936 <- data.frame(
#   sample_name = c(
#     "AU18_0457", "AU18_0359", "AU18_1040", "AU18_0482", "AU18_0388", "AU18_0696", "AU18_0100", "AU18_0961", "AU18_0458",
#     "AU18_0389", "AU18_0456", "AU18_1081", "AU18_1071", "AU18_0254", "AU18_1138", "AU18_0596", "AU18_0742", "AU18_1128",
#     "AU18_0587", "AU18_0614", "AU18_0583", "AU18_0331", "AU18_0251", "AU18_0460", "AU18_0432", "AU18_1083", "AU18_0459",
#     "AU18_0737", "AU18_1126", "AU18_0615", "AU18_0588", "AU18_1129", "AU18_0520", "AU18_1060", "AU18_0111", "AU18_1123",
#     "AU18_0332", "AU18_0963", "AU18_0964", "AU18_0702", "AU18_0589", "AU18_1039", "AU18_0967", "AU18_0617", "AU18_0685",
#     "AU18_1031", "AU18_0688", "AU18_0256", "AU18_1038", "AU18_0612", "AU18_0686", "AU18_1124", "AU18_0260", "AU18_0592",
#     "AU18_0590", "AU18_0910", "AU18_0108", "AU18_0824", "AU18_1079", "AU18_0695", "AU18_0597", "AU18_0692", "AU18_0257",
#     "AU18_0485", "AU18_0895", "AU18_0876", "AU18_0599", "AU18_0745", "AU18_1035", "AU18_1172", "AU18_1033", "AU18_1111",
#     "AU18_1075", "AU18_1072", "AU18_0908", "AU18_1032", "AU18_1010", "AU18_0164", "AU18_0069", "AU18_0071", "AU18_0237",
#     "AU18_0165", "AU18_0064", "AU18_0070", "AU18_0068", "AU18_0067", "AU18_0311", "AU18_0821", "AU18_0748", "AU18_0744",
#     "AU18_0468", "AU18_0248", "AU18_1122", "AU18_1125", "AU18_1041", "AU18_1143", "AU18_0962", "AU18_1058", "AU18_1078",
#     "AU18_0966", "AU18_0483", "AU18_1026", "AU18_0700", "AU18_0474", "AU18_0065", "AU18_0288", "AU18_0063", "AU18_0232",
#     "AU18_0113", "AU18_0162", "AU18_0170", "AU18_0234", "AU18_0163", "AU18_0319", "AU18_0747", "AU18_0220", "AU18_0115",
#     "AU18_1121", "AU18_0965", "AU18_0586", "AU18_0472", "AU18_0463", "AU18_0281", "AU18_0242", "AU18_0632", "AU18_1034",
#     "AU18_0970", "AU18_0469", "AU18_0470", "AU18_0390", "AU18_0387", "AU18_neg12", "AU18_0062", "AU18_0112", "AU18_0072",
#     "AU18_1037", "AU18_0352", "AU18_negEXT", "AU18_neg12-r2", "AU18_neg12-r1", "AU18_neg2", "AU18_0904", "AU18_0743",
#     "AU18_0703", "AU18_0585", "AU18_0484", "AU18_0245", "AU18_0244", "AU18_0243", "AU18_0241", "AU18_0219", "AU18_0218",
#     "AU18_0168", "AU18_0150", "AU18_0114", "AU18_0087", "AU18_0078"
#   ),
# location = c(rep("Heron_Island", 45), rep("far_northern",32), rep("SIMP",25), rep("Lizard",55)),
# collection_latitude = c(rep("151.90665", 45), rep("151.90665", 32), rep("-30.94837",25), rep("-30.94837",55)),
# host_genus = sample(c("Acropora", "Stylophora", "Turbinaria"), 157, replace = TRUE))
# write.csv(metadata_20230120T102936,"metadata_20230120T102936.csv")
#
