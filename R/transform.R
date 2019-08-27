#' Convinient functions for most frequent data transforming needs.

subset_by_quantile <- function(df,
                               col,
                               top = 0,
                               bottom = 0,
                               ...){
  #' remove top and(or) bottom x%
  #' return data.frame(tibble) or numeric vector based on input
  #' col must be string format(with "")

  if(is.data.frame(df)){
  df %>%
    filter(df[[col]] >= quantile(df[[col]], bottom, na.rm=TRUE),
           df[[col]] <= quantile(df[[col]], 1-top, na.rm=TRUE),
           )
  }else{
      upper = quantile(df, bottom, na.rm=TRUE)
      lower = quantile(df, 1-top, na.rm=TRUE)
      df[df >= upper & df <= lower]
  }
}

move_left <- function(df, col) {
  #' take col name or regex and move the column to the left most position
  #' returns a dplyr::tibble object

  col_idx <- grep(col, names(df))
  df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]
}

coalesce_join <- function(x,
                          y,
                          by = NULL,
                          suffix = c(".x", ".y"),
                          join = dplyr::left_join,
                          ...) {
  #join x, y dataframes by appending corresponding y values to where x is missing

  x <- x %>%
    select(-c(names(.)[stringr::str_count(names(.), ".x|.y") >= 1]))

  y <- y %>%
    select(-c(names(.)[stringr::str_count(names(.), ".x|.y") >= 1]))

  joined <- dplyr::join(x, y, by = by, suffix = suffix, ...) %>%
    select(-c(names(.)[stringr::str_count(names(.), ".x|.y") >= 2]))

  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]

  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]

  #remove suffix and remove duplicates
  to_coalesce <- unique(substr(to_coalesce,
                               1,
                               nchar(to_coalesce) - nchar(suffix_used)))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(joined[[paste0(.x, suffix[1])]],
                                                            joined[[paste0(.x, suffix[2])]]))

  name(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}

