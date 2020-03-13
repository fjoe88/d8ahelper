
# Formatting ----------------------------------------------------------------------------------

#' Convert integer/floating point number format to percentage format
#'
#' @param x a number
#' @param digits the desired number of digits after the decimal point
#' @param format default to "f" which gives numbers in the usual xxx.xxx format. see help(formatC)

format_to_percentage <- function(x,
                                 digits = 2,
                                 format = "f",
                                 ...) {

  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#' Trim numbers to 2 digit after decimal point format
#'
#' Accept numerical vectors, or data.frame

format_num <-
  function(num, digits = 2, ...) {

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

#' Format datetime columns to a specified format
#'
#' @param df a data frame
#' @param regex a character used for regular expression lookup of matching columns to format with
#' @param timeformat datetime format to be used for formating columns with (see 'help(strptime)')
#'
#' @example
#' format_datetime(df, "datetime", timeformat = "%m/%d/%Y %H:%M:%S %p")

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
