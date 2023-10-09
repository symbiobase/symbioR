


  alphanum <- c(0:9, letters, LETTERS)

 tmp= data.frame(
    sample.ID =
  c(rep(paste0(sample(alphanum, 4, replace = TRUE), collapse = ""),4),
        rep(paste0(sample(alphanum, 5, replace = TRUE), collapse = ""),5),
            rep(paste0(sample(alphanum, 6, replace = TRUE), collapse = ""),6),
                rep(paste0(sample(alphanum, 7, replace = TRUE), collapse = ""),7),
                    rep(paste0(sample(alphanum, 5, replace = TRUE), collapse = ""),5)),
  seq.ID = c(rep(paste0(sample(alphanum, 2, replace = TRUE), collapse = ""),27)),
  abundance = runif(27, 0, 1)

)

 create_facet_column <- function(df, n) {
   df %>%
     dplyr::group_by(sample.ID) %>%
     dplyr::summarise() %>%
     dplyr::mutate(rn = dplyr::row_number()) %>%
     dplyr::mutate(facet_column = letters[ceiling(rn / (nrow(.) / n))]) %>%
     dplyr::right_join(df, by = "sample.ID")
 }

 tmp2 <- create_facet_column(tmp, 3)
 print(tmp2, n=28)
 tmp2 <- create_facet_column(tmp, 2)
 print(tmp2, n=28)



