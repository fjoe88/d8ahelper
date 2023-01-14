


# transform -----------------------------------------------------------------------------------

#' Remove leading and trailing white spaces
#'
#' By default, trim both leading and traling white spaces unless specified
#' @param x a character
#' @param all a boolean, trim non-leading/trailing white spaces as well if equals TRUE
#' @param replacement a character to replace non-leading/trailing white spaces with

trim_ws <- function(x,
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

#' Remove leading and trailing whitespaces that are within any character or factor type columns of a data.frame

trim_ws_df <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) | is.factor(col)) {
      col <- sapply(col, trimws)
    }

    return(col)
  })

  return(df)
}


#' Move column or columns to far left hand side of a dataframe
#'
#' Accept multiple columns if passed in a character vector containing column names
#' @param df a data frame
#' @param str column name(s)
#' @examples
#' move_left(mtcars, "wt")
#' move_left(mtcars, c("gear", "carb", "wt"))

move_left <- function(df, str) {
  str <- intersect(str, names(df)) #to preserve order by str
  if (length(str) >= 1) {
    col.indx.part1 <- sapply(seq_along(str), function(i) {
      which(str[i] == names(df))
    })

    col.indx.part2 <-
      seq_len(ncol(df))[!seq_len(ncol(df)) %in% col.indx.part1]
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


# join -----------------------------------------------------------------------------------

#' Coalescence join x, y data frames, for columns of same names, append y values to x if rows that are missing value
#'
#' @param x
#' @param y
#' @param by
#' @param suffix
#' @param join
#' @param ...

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


#' Remove column(s) that contain a single unique value

rm_single_unique_col <- function (df,
                                  threshold = 1,
                                  exclude = c())
{
  if (length(df) <= 1 | !is.data.frame(df)) {
    return(df)
  }

  df <- data.table::as.data.table(df)

  ids <- which(detect_id(df, combine_result = T))

  if (length(exclude) > 0) {
    ids_excl <- which(names(df) %in% exclude)
    ids <- union(ids, ids_excl)
  }

  df1 <- df[, ids, with = FALSE]
  rest <- setdiff(seq_along(df), ids)
  df2 <- df[, rest, with = FALSE]
  df2 <-
    df2[,!sapply(df2, function(x) {
      all(is.na(x))
    }), with = FALSE]
  uniq <- sapply(df2, function(x) {
    length(unique(x[!is.na(x)]))
  })
  cbind(df1, df2[,!uniq %in% c(0:threshold), with = FALSE])
}


#' Remove row(s) if all values are NAs

remove_empty_rows <- function(df) {
  if (!is.data.frame(df))
    stop("not a data frame")
  df %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))
}

#' Remove duplicated rows by leaving only 1 observation per each group of most non-missing data columns
#' if multiple observations sharing equal number of most non-missing data columns, choose the first one by row index
#' a wrapper function of d8ahelper::any_dups
#' @param keys a character vector contains column names, for grouping purposes
rm_dups_w_less_data <- function(df, keys) {
  df <- data.table::as.data.table(df)

  dups <- any_dups(df, keys = keys)

  dt <- data.table::as.data.table(df)

  dt.uniq <- dt[!dups$dup_row_bool,]

  dt.dup <- dups$dups

  not_missing <- sapply(dt.dup[,-keys, with = FALSE], function(col) {
    is.na(col) | col == ""
  })
  sums <-
    apply(not_missing, 1, sum) #faster than using apply row-wise with test

  #if multiple max exist, return first one
  .first_max <- function(...) {
    if (!is.numeric(c(...))) {
      stop('only accept numerics')
    }

    bool <- c(...) == max(...)

    if (sum(bool) > 1) {
      bool[which(bool)[-1]] <- FALSE
    }

    return(bool)
  }

  dt.dup$.sums <- sums

  dt.dup <- dt.dup[dt.dup[, .I[.first_max(.sums)], by = keys]$V1]

  dt.dup[, .sums := NULL]

  message(
    glue::glue(
      "{nrow(dups$dups)-nrow(dt.dup)} rows with duplicated keys({paste(dups$key, collapse=',')}) removed."
    )
  )

  rbind(dt.uniq, dt.dup)

}



# missing, NAs --------------------------------------------------------------------------------

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

#' Remove duplicated rows of the original data frame, or a subset of if column names being passed in

#' Remove NAs given a vector
#'
rm_na <- function(x) {
  x[!is.na(x)]
}

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
    print(glue::glue(
      "Output dataframe of {nrow(df5)} rows containing no duplicates"
    ))
    df5[,!(names(df5) == "count")]

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
    print(glue::glue(
      "Output dataframe of {nrow(df5)} rows containing no duplicates"
    ))
    df5[,!(names(df5) == "count")]

  }
}

#'Fill NAs and empty cells with fillers such as a character
#'
fill_na_as_missing <- function(df, fill = "[missing]") {
  if (!is.data.frame(df)) {
    message('not a dataframe, return original')
    return(df)
  }

  list_of_cols <-
    d8ahelper::lapply_preserve_names(df, function(col) {
      if (is.character(col) | is.factor(col)) {
        col[sapply(col, function(x) {
          any(is.na(x), any(grep("^[[:space:]]*$", x)), x == 'NA')
        })] <- fill
      }

      return(as.data.frame(col))

    })

  return(do.call(cbind, list_of_cols))
}

#' Sister function to fill_na_as_missing, replace cells with NAs if match to given string

fill_as_na <- function(df, pattern = "[missing]") {
  df[df == pattern] <- NA
  df
}


#' Artificially increase the number of rows to make up any missing combinations by label columns for every unique id
#' 'Puff up' the dataframe by filling in the missing entries with artificial data, default NA
#' Will move id columns and label columns to the far left
#' tidyr::crossing and tidyr::expand.grid perform similar function but do require loading up tidyr package first
#'

puff_my_df <- function(df, id_col, label_col, fill_with = NA) {
  df <- as.data.frame(df)

  any_missing <- Reduce("|", lapply(df[id_col], is.na))
  df[any_missing, ] <- NULL

  label <- unique(df[, label_col])

  swap_flag <- FALSE

  if (length(id_col) > 1) {
    swap_flag <- TRUE

    combined_id_col <-
      Reduce(function(x, y) {
        paste(x, y, sep = "_")
      }, df[, id_col])
    new_id <- paste(id_col, collapse = "_")
    df[[new_id]] <- combined_id_col
  } else {
    new_id <- id_col
  }

  #pb <- progress::progress_bar$new(total = nrow(unique(df[new_id])))

  l <- lapply(unique(df[[new_id]]), function(x) {
    #pb$tick()

    upper <- df[df[[new_id]] == x, ]
    label_col_lower <- setdiff(label, upper[label_col])

    if (nrow(label_col_lower) == 0) {
      return(upper)
    }

    if (swap_flag == TRUE) {
      upper <- move_left(upper, c(new_id, id_col, label_col))

      lr_ncol <-
        ncol(df) - length(c(new_id, id_col, label_col))


    } else {
      upper <- move_left(upper, c(new_id, label_col))

      lr_ncol <- ncol(df) - length(c(new_id, label_col))

    }


    #lower right artificial data
    lr <-
      do.call(cbind,
              rep(list(rep(
                fill_with, nrow(label_col_lower)
              )),
              lr_ncol))

    #if id_col is of multiples, preserve original id_cols in the lower part as well
    if (swap_flag == TRUE) {
      lower <- cbind(rep(x, nrow(label_col_lower)),
                     sapply(unique(upper[id_col]), function(x)
                       rep(x, nrow(
                         label_col_lower
                       ))),
                     label_col_lower,
                     lr)
    }

    if (swap_flag == FALSE) {
      lower <- cbind(rep(x, nrow(label_col_lower)),
                     label_col_lower,
                     lr)
    }

    names(lower) <- names(upper)

    result <- rbind(upper, lower)

    if (swap_flag == TRUE) {
      result$new_id <- NULL
    }

    return(result)
  })

  do.call(rbind, l)
}

clean_by_id <-
  function(df,
           id_col,
           var_col = NULL,
           filter_col = FALSE,
           th = 1) {
    #'Remove rows where id columns are all missing, and rows where all columns but the id columns are missing; (optional) Remove columns where all rows are of missing values.
    #'@param id_col numeric vector specify which columns are considered id columns
    #'@param var_col numeric vector specify which columns are considered data columns
    #'@param filter_col a bool, if TRUE then will leave only columns that does not contain missing - applies to all non id columns if var_col is NULL otherwise only applies to var_col only.
    #'@param th a numeric, range from 0-1 to specify the threshold for amount least amount of non-missing data by column in fractions of total rows

    dt <- data.table::as.data.table(df)

    #row operation by id cols
    row_no_ids <-
      rowSums(is.na(dt[, id_col, with = FALSE])) == length(id_col)

    if (any(row_no_ids)) {
      dt <- dt[-row_no_ids, ]
    }

    if (is.null(var_col)) {
      rhs <- dt[,-id_col, with = FALSE]
    }

    if (!is.null(var_col)) {
      rhs <- dt[, var_col, with = FALSE]
    }

    #row operation to rhs
    row_all_na <- apply(rhs, 1, function(x) {
      all(is.na(x))
    })

    dt <- dt[!row_all_na,]

    if (filter_col == FALSE) {
      rhs <- rhs[!row_all_na,]
    }

    #column operation to rhs
    if (filter_col == TRUE) {
      #rhs may be different since entire-empty rows were removed
      if (is.null(var_col)) {
        rhs <- dt[,-id_col, with = FALSE]
      }

      if (!is.null(var_col)) {
        rhs <- dt[, var_col, with = FALSE]
      }

      non_miss_pct <- sapply(rhs, function(x) {
        sum(!(is.na(x) | grepl(x, pattern = "//s+") | x == "")) / length(x)
      })

      rhs[, which(non_miss_pct < th)] <- NULL
    }

    if (is.null(rhs)) {
      message("rhs returns NULL, all columns contain at least 1 missing value")
    }

    dt <- cbind(dt[, id_col, with = FALSE], rhs)

    return(dt)
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
