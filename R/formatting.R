#' Helper functions for general data processing at IM Flash.


# Formatting ----------------------------------------------------------------------------------


format_to_percentage <- function(x,
                                 digits = 2,
                                 format = "f",
                                 ...) {
  #'given a integer/floating point number, convert to 'dd.dd%' percentageformat

  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

format_num <-
  function(num, digits = 2, ...) {
    #' trim numbers to 2 digit after decimal point format
    #' accept number vectors or data.frame

    if (is.data.frame(num)) {
      is.num <- sapply(num, is.numeric)
      num[is.num] <- lapply(num[is.num], function(x) {
        as.double(format(round(x, digits), nsmall = digits))
      })
      return(num)
    } else{
      as.double(format(round(as.numeric(num), digits), nsmall = digits))
    }
  }

format_datetime <- function(df, regex, timeformat, ...) {
  if (is.data.frame(df)) {
    for (col in names(df)[grep(regex, names(df))]) {
      df[[col]] <- readr::parse_datetime(raw1[[col]], format = timeformat)
    }
  } else{
    df <- readr::parse_datetime(df, format = timeformat)
  }
  return(df)
}
