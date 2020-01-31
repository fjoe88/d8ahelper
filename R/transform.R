
# Transform -----------------------------------------------------------------------------------

#' Trim leading and trailing white spaces
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


#' Insert NAs randomly to a data.frame
#'
#' @param df a data frame
#' @param percent percent of NAs per column
#' @return a data frame
#' @example
#' foo <- insert_nas(mtcars, percent = 0.1)

insert_nas <- function(df, percent = 0.2) {
  as.data.frame(lapply(df, function(x) {
    "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, percent))))
  })
    )
}

#' Move column to far left hand side
#'
#' Accept multiple columns if passed in a character vector containing column names
#' @param df a data frame
#' @param str column name(s)
#' @examples
#' View(move_left(mtcars, "wt"))
#' View(move_left(mtcars, c("gear", "carb", "wt")))

move_left <- function(df, str) {

  if (length(str) == 1) {
    col_idx <- grep(str, names(df))
    df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]
  } else if (length(str) > 1) {
    col_idx <- integer()
    for (i in seq_along(str)) {
      idx <- grep(str[i], names(df))
      col_idx[i] <- idx
    }
    df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]
  }
}

#' Remove duplicated rows of the original data frame, or a subset of if column names being passed in

remove_duplicates <- function(df, ...) {
  if (missing(...)) {
    print(names(df))
    df1 <- unique(df[,])
    df2 <- purrr::transpose(df1)

    df3 <- purrr::transpose(df[,])

    df4 <-
      as.data.frame(sapply(df2, function(m) {
        sum(sapply(df3, function(n)
          identical(m, n)))
      }))

    names(df4) <- "count"
    df5 <- cbind(df4, df1)
    df6 <- df5[order(df5$count, decreasing = TRUE),]
    df7 <- df6[which(df6$count > 1),]

    print(glue::glue("Total of {nrow(df)-nrow(df5)} duplicated rows:"))
    print(d8ahelper::headtail(df7))
    print(glue::glue("Output dataframe of {nrow(df5)} rows containing no duplicates"))
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
    df6 <- df5[order(df5$count, decreasing = TRUE),]

    df7 <- df6[which(df6$count > 1),]

    print(glue::glue("Total of {nrow(df)-nrow(df5)} duplicated rows:"))
    print(d8ahelper::headtail(df7))
    print(glue::glue("Output dataframe of {nrow(df5)} rows containing no duplicates"))
    df5[, !(names(df5) == "count")]

  }
}


add_datehour <- function(df, datecol = "trackedout", ...) {
  #generage Date and Hour column based on Datetime column

  df$Date <- lubridate::as_date(df[[datecol]])
  df$Hour <- lubridate::hour(df[[datecol]])
  df <- df %>%
    mutate("DateHour" = paste0(Date, "-", Hour))

  df$Date <- as.factor(df$Date)
  df$DateHour <- as.factor(df$DateHour)
  return(df)
}

add_wmy <- function(df, dt_col) {
  #add week, weekday, month, year columns

  if (is.POSIXct(df[[dt_col]])) {
    df$dWeek <- as.factor(weekdays(df[[dt_col]]))

    df$WW <- lubridate::isoweek(df[[dt_col]] - 327600)
    df$WW <- as.factor(as.character(sprintf("%02d", df$WW)))

    df$Month <- as.factor(lubridate::month(df[[dt_col]]))

    df$Year <- as.factor(lubridate::year(df[[dt_col]]))

    return(df)
  }
}

# Removing

subset_by_quantile <- function(df,
                               col,
                               top = 0,
                               bottom = 0,
                               ...) {
  #remove top% and(or) bottom%
  df %>%
    filter(df[[col]] >= quantile(df[[col]], bottom, na.rm = TRUE),
           df[[col]] <= quantile(df[[col]], 1 - top, na.rm = TRUE))
}



# Data Processing

#' Join x, y data frames by appending y values to x if missing (for columns of same names)

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

#' Append empty rows to a dataframe to make row number = n

add_empty_rows <- function(df, n) {

  new.row <- rep(NA, length = ncol(df))
  new.row <- rbind(new.row)
  m <- length(df[[1]])
  to.append <-
    as.data.frame(new.row[rep(1:nrow(new.row), length = n - m), , drop = FALSE])
  names(to.append) <- names(df)
  new.df <- rbind(df, to.append, use.names = FALSE)
}

#' Convert time related columns to character
#'
#' background: POSIXct datetime format when output as csv will not be read corretly with JMP \cr
#' purpose: convert all time columns to character format befor exporting to csv \cr
#' note: accept regex if default rule of selecting columns containing "time" is not ideal \cr
#'
#' @param df a data frame
#' @param regex a regular expression character vector, to be used for matching to 'time' columns

convert_time_to_chr <- function(df, regex = "Time") {

  df %>% dplyr::mutate_at(vars(contains(!!regex)), as.character)
}

#' Remove column(s) if containing a single unique value

rm_single_unique_col <- function(df,
                                 threshold = 1) {

  df1 <- df[grepl(pattern = "^startlot$|^startlotkey$|^fulllot$|^lotid$|^lot_id$|^waferid$|^alias$", tolower(names(df)))]
  df2 <- df[!grepl(pattern = "^startlot$|^startlotkey$|^fulllot$|^lotid$|^lot_id$|^waferid$|^alias$", tolower(names(df)))]

  uniq <- sapply(df2, function(x) {
    length(unique(x[!is.na(x)]))
  })

  cbind(df1, df2[, !uniq %in% c(0:threshold)])
}

#' Remove row(s) if all values are NAs

remove_empty_rows <- function(df) {
  if (!is.data.frame(df)) stop("not a data frame")
  df %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))
}

#' Fill NAs with fillers

fill_na_as_missing <- function(df,
                               fill = "[missing]",
                               convert_fct_to_chr = TRUE) {
  if (!is.data.frame(df)) {
    stop("only accept a data frame")
  }

  if (convert_fct_to_chr == TRUE) {
    df[] <- lapply(df, function(col) {
      if (is.factor(col)) {
        col <- as.character(col)
      } else{
        col
      }

    })
  }

  dplyr::mutate_if(df, is.character, list(~ tidyr::replace_na(., fill)))
}
