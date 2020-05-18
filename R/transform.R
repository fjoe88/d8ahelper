

# transform -----------------------------------------------------------------------------------

#' Remove leading and trailing white spaces
#'
#' By default, trim both leading and traling white spaces unless specified
#' @param x a character
#' @param all a boolean, trim non-leading/trailing white spaces as well if equals TRUE
#' @param replacement a character to replace non-leading/trailing white spaces with

trim_spaces <- function(x,
                        leading = FALSE,
                        trailing = FALSE,
                        all = FALSE,
                        replacement = "_") {
  if (leading == FALSE &&
      trailing == FALSE) {
    x <- gsub("^\\s+|\\s+$", "", x)
  }
  else if (leading == TRUE) {
    x <- gsub("^\\s+", "", x)
  }
  else if (trailing == TRUE) {
    x <- gsub("\\s+$", "", x)
  }

  if (all == FALSE) {
    return(x)
  } else{
    x <- gsub("^\\s+|\\s+$", "", x)
    x <- gsub("\\s+", " ", x)
    x <- gsub(" ", x, replacement = replacement)
    return(x)
  }

}

#' Move column or columns to far left hand side of a dataframe
#'
#' Accept multiple columns if passed in a character vector containing column names
#' @param df a data frame
#' @param str column name(s)
#' @examples
#' View(move_left(mtcars, "wt"))
#' View(move_left(mtcars, c("gear", "carb", "wt")))

move_left <- function(df, str) {
  str <- intersect(names(df), str)
  if (length(str) >= 1){
    col.indx.part1 <- sapply(seq_along(str), function(i) {
      which(str[i] == names(df))
    })

    col.indx.part2 <- seq_len(ncol(df))[!seq_len(ncol(df)) %in% col.indx.part1]
    df <- df[, c(col.indx.part1, col.indx.part2)]
  } else {
    message("No matching columns found, return original data frame")
    return(df)
  }
}

#Add week, weekday, month, year columns based on datetime column

add_wmy <- function(df, dt_col) {


  if (is.POSIXct(df[[dt_col]])) {
    df$dWeek <- as.factor(weekdays(df[[dt_col]]))

    df$WW <- lubridate::isoweek(df[[dt_col]] - 327600)
    df$WW <- as.factor(as.character(sprintf("%02d", df$WW)))

    df$Month <- as.factor(lubridate::month(df[[dt_col]]))

    df$Year <- as.factor(lubridate::year(df[[dt_col]]))

    return(df)
  }
}

# Filter column by removing points beyond top and(or) bottom percentage thresholds

subset_by_quantile <- function(df,
                               col,
                               top = 0,
                               bottom = 0,
                               ...) {

  df %>%
    filter(df[[col]] >= quantile(df[[col]], bottom, na.rm = TRUE),
           df[[col]] <= quantile(df[[col]], 1 - top, na.rm = TRUE))
}

# join -----------------------------------------------------------------------------------

#' Coalescely join x, y data frames, for columns of same names, append y values to x if rows that are missing value

coalesce_join <- function(x,
                          y,
                          by = NULL,
                          suffix = c(".x", ".y"),
                          join = dplyr::left_join,
                          ...) {
  x <- x %>%
    select(-c(names(.)[str_count(names(.), ".x|.y") >= 1]))

  y <- y %>%
    select(-c(names(.)[str_count(names(.), ".x|.y") >= 1]))

  joined <- join(x, y, by = by, suffix = suffix, ...) %>%
    select(-c(names(.)[str_count(names(.), ".x|.y") >= 2]))

  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <-
    suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(to_coalesce,
                               1,
                               nchar(to_coalesce) - nchar(suffix_used)))

  coalesced <-
    purrr::map_dfc(to_coalesce, ~ dplyr::coalesce(joined[[paste0(.x, suffix[1])]],
                                                  joined[[paste0(.x, suffix[2])]]))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}

#' Wrapper function to coalesce_join, join together a list of data frames

multi_join <- function(list,
                       by = NULL,
                       join = dplyr::full_join,
                       ...) {
  Reduce(function(x, y, ...) {
    d8ahelper::coalesce_join(x, y, by = by, join = join, ...)
  },
  list)
}



# redundant rows/columns ----------------------------------------------------------------------


#' Remove column(s) if containing a single unique value

rm_single_unique_col <- function(df,
                                 threshold = 1) {

  if (length(df) <= 1 | !is.data.frame(df)) {return(df)}

  ids <- wafer::detect_all(df, return_loc = TRUE)
  df1 <- df[, ids]

  rest <- setdiff(seq_along(df), ids)
  df2 <- df[, rest]

  #remove all NA columns
  df2 <- df2[!sapply(df2, function(x) all(is.na(x)))]

  #filter columns by threshold
  uniq <- sapply(df2, function(x) {
    length(unique(x[!is.na(x)]))
  })

  cbind(df1, df2[, !uniq %in% c(0:threshold)])
}

#' Remove row(s) if all values are NAs

remove_empty_rows <- function(df) {
  if (!is.data.frame(df))
    stop("not a data frame")
  df %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))
}

# missing, NAs --------------------------------------------------------------------------------

#' Remove duplicated rows of the original data frame, or a subset of if column names being passed in

#' Remove NAs given a vector
#'
rm_na <- function(x) {
  x[!is.na(x)]
}

remove_duplicates <- function(df, ...) {
  if (missing(...)) {
    print(names(df))
    df1 <- unique(df[, ])
    df2 <- purrr::transpose(df1)

    df3 <- purrr::transpose(df[, ])

    df4 <-
      as.data.frame(sapply(df2, function(m) {
        sum(sapply(df3, function(n)
          identical(m, n)))
      }))

    names(df4) <- "count"
    df5 <- cbind(df4, df1)
    df6 <- df5[order(df5$count, decreasing = TRUE), ]
    df7 <- df6[which(df6$count > 1), ]

    print(glue::glue("Total of {nrow(df)-nrow(df5)} duplicated rows:"))
    print(d8ahelper::headtail(df7))
    print(glue::glue(
      "Output dataframe of {nrow(df5)} rows containing no duplicates"
    ))
    df5[, !(names(df5) == "count")]

  } else {
    df1 <- unique(df[, c(...)])
    df2 <- purrr::transpose(df1)

    df3 <- purrr::transpose(df[, c(...)])

    df4 <-
      as.data.frame(sapply(df2, function(m) {
        sum(sapply(df3, function(n)
          identical(m, n)))
      }))

    names(df4) <- "count"
    df5 <- cbind(df4, df1)
    df6 <- df5[order(df5$count, decreasing = TRUE), ]

    df7 <- df6[which(df6$count > 1), ]

    print(glue::glue("Total of {nrow(df)-nrow(df5)} duplicated rows:"))
    print(d8ahelper::headtail(df7))
    print(glue::glue(
      "Output dataframe of {nrow(df5)} rows containing no duplicates"
    ))
    df5[, !(names(df5) == "count")]

  }
}

#'Fill NAs and empty cells with fillers such as a character
#'
fill_na_as_missing <- function(df,
                               fill = "[missing]") {
  if (!is.data.frame(df)) {
    message('not a dataframe, return original')
    return(df)
  }

  as.data.frame(sapply(df, function(col) {
    if (is.character(col) | is.factor(col)) {
      col[sapply(col, function(x) {
        any(is.na(x), any(grep("^[[:space:]]*$", x)), x == 'NA')
      })] <- fill
    }

    return(col)

  }))
}

#' Sister function to fill_na_as_missing, replace cells with NAs if match to certain string

fill_missing_as_na <- function(df, pattern = "[missing]") {


  df[df == pattern] <- NA
  df
}

# column names --------------------------------------------------------------------------------

#' Replace column names with alpha-numeric sequences, returns a named vector (of name-value pairs) for column name look-up; Returns a list.

encode_col <- function(df) {


  dict <- names(df)
  names(dict) <- paste0("c", seq_along(df))

  names(df) <- paste0("c", seq_along(df))

  return(list("tag" = dict,
              "df" = df))

}

#' Sister function to encode_col
#' returns a data frame with original column names based on df$dict
#'
#' @param df.list: list object generated from d8ahelper::convert_col_name
#' @param df: data frame to convert names from, default to df within df.list

decode_col <- function(df.list, df = df.list$df) {


  names(df) <- sapply(names(df), function(x)
    df.list$tag[[x]])
  df
}

get_name <- function(df, ...) {
  #' sister function to encode_col, retrieve column names based on id(s)

  sapply(..., function(x)
    df$tag[[x]])
}

get_id <- function(df, ...) {
  #' sister function to encode_col, retrieve column id based on name(s)

  sapply(..., function(x)
    names(df$tag)[df$tag == x])
}

# regular expressions -------------------------------------------------------------------------

str_find <- function(str,
                     start,
                     end,
                     replacement,
                     return_loc = FALSE) {
  #'wrapper function for locate a string based on regex leading to and after it
  #'@param str a string to search from
  #'@param start a regex string that lead to the string of interest
  #'@param end a regex string that is after the strin gof interest
  #'@param replacement a string to replace the located string, optional.
  #'@return_loc a logical, if TRUE then return character start/end location instead of the string itself
  #'
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

# Others --------------------------------------------------------------------------------------


#' Append empty rows to a dataframe to make row number a target number
#'
#' @param n a numeric, the number of rows of the outcome table after filling missing rows

add_empty_rows <- function(df, n) {
  new.row <- rep(NA, length = ncol(df))
  new.row <- rbind(new.row)
  m <- length(df[[1]])
  to.append <-
    as.data.frame(new.row[rep(1:nrow(new.row), length = n - m), , drop = FALSE])
  names(to.append) <- names(df)
  new.df <- rbind(df, to.append, use.names = FALSE)
}


#' Insert(inject) NAs as replacement randomly to a data
#'
#' @param df a data frame
#' @param percent percent of NAs per column
#' @return a data frame
#' @example
#' foo <- insert_nas(mtcars, percent = 0.1)

insert_nas <- function(df, percent = 0.2) {
  as.data.frame(lapply(df, function(x) {
    "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, percent))))
  }))
}
