#' Convinient functions for most frequently used data transferring needs.

copy <- function(df,
                 col,
                 sep = ",",
                 quotes = FALSE,
                 ...) {
  #' if passed in a single string: remove all spaces (comma delimited by default)
  #' if passed in a column from dataframe or tibble: copy unique values to clipboard (comma delimited by default)

  if (.Platform$OS.type == "windows") {
    #' use utils::writeClipboard if under windows based OS

    if (is.data.frame(df)) {
      unique_vals <- unique(df[[col]])
      str_to_copy <- paste(unique_vals, collapse = sep)
      utils::writeClipboard(str_to_copy)
    } else{
      if (length(df) == 1) {
        str_to_copy <- gsub("[[:space:]]+", ",", df)
        utils::writeClipboard(str_to_copy)
      } else{
        unique_vals <- unique(df)
        if (quotes == TRUE) {
          unique_vals <- paste0("\"", unique_vals, "\"", sep = "")
        }
        str_to_copy <- paste(unique_vals, collapse = sep)
        utils::writeClipboard(str_to_copy)
      }
    }
  } else {
    if (.Platform$OS.type == "unix") {
      #' use clipr::write_clip if under unix based OS

      if (is.data.frame(df)) {
        unique_vals <- unique(df[[col]])
        str_to_copy <- paste(unique_vals, collapse = sep)
        clipr::write_clip(str_to_copy)
      } else{
        if (length(df) == 1) {
          str_to_copy <- gsub("[[:space:]]+", ",", df)
          clipr::write_clip(str_to_copy)
        } else{
          unique_vals <- unique(df)
          if (quotes == TRUE) {
            unique_vals <- paste0("\"", unique_vals, "\"", sep = "")
          }
          str_to_copy <- paste(unique_vals, collapse = sep)
          clipr::write_clip(str_to_copy)
        }
      }
    }
  }
}
