
# Formatting ----------------------------------------------------------------------------------

#' Convert integer/floating point number format to percentage format
#'
#' @param x a number
#' @param digits the desired number of digits after the decimal point
#' @param ...
#' @param format default to "f" which gives numbers in the usual xxx.xxx format. see help(formatC)
#'
#' @examples
#' format_to_percentage(0.32)

format_to_percentage <- function(x,
                                 digits = 2,
                                 format = "f",
                                 ...) {

  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#' Format numbers to limit digits after decimal point
#'
#' Accept numerical vectors, or data.frame
#'
#' @param num
#' @param digits
#' @param ...
#'
#' @examples
#' format_num(3.1415926)
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
#' format_datetime(data.frame(a=c(1,2), date=c(Sys.time(), Sys.time()-3600)), "date")
format_datetime <- function (df,
                             regex,
                             timeformat = "%Y-%m-%d %H:%M:%S",
                             ...)
{
  if (is.data.frame(df)) {
    for (col in names(df)[grep(regex, names(df))]) {
      if(typeof(df[[col]])!="character"){
        df[[col]] <- as.character(df[[col]])
      }
      df[[col]] <- readr::parse_datetime(df[[col]],
                                         format = timeformat)
    }
  }
  else {
    df <- readr::parse_datetime(df, format = timeformat)
  }
  return(df)
}

#' Capitalize the first letter of each word in the string
#'
#' @example
#' cap_str("a WOrd CAPITALIZED,   with extra   spaces,with no leading space")

cap_str <- function(x, collapse = " ") {

  x <- tolower(x)
  x <- strsplit(x, ",", perl = TRUE)[[1]]
  x <- paste(x, collapse = ", ")
  x <- strsplit(x, "[[:space:]]+", perl = TRUE)[[1]]

  paste(toupper(substring(x, 1,1)),
        substring(x, 2),
        sep="",
        collapse=collapse)
}

#'convert all columns of factor type to character type
conv_fct_to_chr <- function(df) {
  as.data.frame(sapply(df, function(col) {
    if (is.factor(col)) {
      col <- as.character(col)
    }

    return(col)

  }))
}

#'convert all columns of character type to factor type
#'
#' @param df
#'
#' @examples
#' conv_chr_to_fct(data.frame(a=c("a","a","b"),b=c(1,2,3)))

conv_chr_to_fct <- function(df) {
  as.data.frame(sapply(df, function(col) {
    if (is.character(col)) {
      col <- as.factor(col)
    }

    return(col)

  }))
}
# Date and time -----------------------------------------------------------


#' Convert numerically converted format POSIXct time back to POSIXct datetime format
#'
#' @param secs
#'
#' @examples
#' secs_to_date(1622342600)
secs_to_date <- function(secs) {
  as.POSIXct(secs, origin = "1970-01-01")
}

#' Use number of days past 1970-01-01 to indicate date
#'
#' @param days
#'
#' @return
#' @export
#'
#' @examples
#' days_to_date(30)
days_to_date <- function(days){
  secs <- days*(24*60*60)
  as.POSIXct(secs, origin = "1970-01-01")
}
