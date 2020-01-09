#' Convinient functions for most frequently used data transferring needs.

copy_unique <- function(df = paste(readClipboard(), collapse = ","),
                 col = "lotid",
                 sep = ",",
                 quotes = FALSE,
                 format_lotid = TRUE,
                 save_csv = FALSE,
                 ...) {
  #'copy unique values to clipboard
  #'
  #'input: chr str copied from clipboard, or a data.frame
  #' 
  #'output:
  #'comma separated string w/ duplicates removed
  #'a data.frame of unique values in each row
  #'
  #' if input is a data.frame, do not return back as a data.frame
  #' by default, assume lot ids being copied and format them unless set format_lot = FALSE
  #' if save_csv = TRUE, save output to '1.lot_list.csv'
 
  stopifnot(.Platform$OS.typ %in% c("windows", "unix"))

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
