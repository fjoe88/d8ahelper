#' Convinient functions for most frequent data formatting needs.

require(lubridate)
require(data.table)
require(tidyverse)
require(modelr)


format_as_percent <- function(num, digits = 2, format="f", ...) {
  #convert number to percentage format
  paste0(formatC(100 * num, format = format, digits = digits, ...), "%")
}

format_number <- function(x, digits = 2, ...) {
  if (is.data.frame(x)) {
    is.num <- sapply(x, is.numeric)
    x[is.num] <- lapply(x[is.num], function(x){
      as.double(format(round(x, digits), nsmall = digits))
    })
    return(x)
  }else{
    as.double(format(round(as.numeric(num), digits), nsmall = digits))
  }
}

format_datetime <- function(x, regex, timeformat, ...) {
  if(is.data.frame(x)) {
    for (col in names(x)[grep(regex, names(x))]) {
      x[[col]] <- readr::parse_datetime(raw[[col]], format = timeformat)
    }
  }else{
    x <- readr::parse_datetime(x, format = timeformat)
  }
  return(x)
}




