# regular expressions -------------------------------------------------------------------------
#'wrapper function for locate a string based on regex leading to and after it
#'@param str a string to search from
#'@param start a regex string that lead to the string of interest
#'@param end a regex string that is after the string of interest
#'@param replacement a string to replace the located string, optional.
#'@return_loc a logical, if TRUE then return character start/end location instead of the string itself
#'@export
#'
#'@example
#'str_find("something HaHaFINDMEended string.", "HaHa", "ended", return_loc=T)
#'str_find("something HaHaFINDMEended string.", "HaHa", "ended", "FOUNDED!")
str_find <- function(str,
                     start,
                     end,
                     replacement,
                     return_loc = FALSE) {
  start_loc <- stringr::str_locate(str, start)[[2]] + 1
  end_loc <- stringr::str_locate(str, end)[[1]] - 1

  if (return_loc == TRUE) {
    return(c("start" = start_loc,
             "end" = end_loc))
  }

  if (hasArg(replacement)) {
    part1 <- substr(str, 1, start_loc - 1)
    part2 <- replacement
    part3 <- substr(str, end_loc + 1, nchar(str))
    return(paste0(part1, part2, part3))
  }
  if (!hasArg(replacement)) {
    return(substr(str, start_loc, end_loc))
  }
}
