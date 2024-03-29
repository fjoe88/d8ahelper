

#' Remove leading and trailing white spaces
#'
#' By default, trim both leading and traling white spaces unless specified
#' @param x a character
#' @param all a boolean, trim non-leading/trailing white spaces as well if equals TRUE
#' @param replacement a character to replace non-leading/trailing white spaces with
#' @export
#' @example
#' trim_ws("   a Chelsea player   ")
#' trim_ws("   Didier Drogba   ", all=T, replacement = "-")

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
#'
#' @param df a data.frame
#' @export
#' @example
#' trim_ws_df(data.frame(a=c(1,2,3),b=c("  f","o   ", "  o")))

trim_ws_df <- function(df) {
  if (!is.data.frame(df)) {
    stop("[trim_ws_df]: input not a dataframe")
  }
  as.data.frame(lapply(df, d8ahelper::trim_ws))
}


#' Move column or columns to far left hand side of a dataframe
#'
#' Accept multiple columns if passed in a character vector containing column names
#' @param df a data frame
#' @param str column name(s)
#' @export
#' @example
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



#' Add week, weekday, month, year columns based on datetime column
#'
#' @param df
#' @param dt_col
#'
#' @return
#' @export
#'
#' @example
#' add_wmy(data.frame(date=Sys.Date()), dt_col = "date")

add_wmy <- function(df, dt_col) {
  if (lubridate::is.Date(df[[dt_col]])) {
    df$dWeek <- as.factor(weekdays(df[[dt_col]]))

    df$WW <- lubridate::isoweek(df[[dt_col]] - 327600)
    df$WW <- as.factor(as.character(sprintf("%02d", df$WW)))

    df$Month <- as.factor(lubridate::month(df[[dt_col]]))

    df$Year <- as.factor(lubridate::year(df[[dt_col]]))

    return(df)
  }
}



#' Filter column by removing points beyond top and(or) bottom percentage thresholds
#'
#' @param df
#' @param col
#' @param top
#' @param bottom
#' @param ...
#'
#' @return
#' @export
#'
#' @example
#' subset_by_quantile(data.frame(a=1:100),col="a", top = 0.2, bottom =0.5)

subset_by_quantile <- function(df,
                               col,
                               top = 0,
                               bottom = 0,
                               ...) {
  df <-
    dplyr::filter(df,
                  df[[col]] >= quantile(df[[col]], bottom, na.rm = TRUE),
                  df[[col]] <= quantile(df[[col]], 1 - top, na.rm = TRUE))
  return(df)

}


#' Append empty rows to a dataframe to make row number a target number
#'
#' @param df
#' @param n a numeric, the number of rows of the outcome table after filling missing rows
#' @export
#' @example
#' add_empty_rows(data.frame(a=c(1,2,3), b=c("a","b","c")), n = 5)

add_empty_rows <- function(df, n) {
  new.row <- rep(NA, length = ncol(df))
  new.row <- rbind(new.row)
  m <- length(df[[1]])
  to.append <-
    as.data.frame(new.row[rep(1:nrow(new.row), length = n - m), , drop = FALSE])
  names(to.append) <- names(df)
  new.df <- rbind(df, to.append)
  return(new.df)
}


# join -----------------------------------------------------------------------------------

#' Coalescence join lhs x and rhs y data frames, for each column-row combination, append rhs to lhs if missing, use lhs value if there are conflicts
#'
#' @param x
#' @param y
#' @param by
#' @param suffix
#' @param join
#' @param ...
#'
#' @export
#' @example
#' df1 <- data.frame(id=c("a","a","b","c"), id2=c(1,2,1,1), data=c(1,NA,NA,2))
#' df2 <- data.frame(id=c("a","a","b","c"), id2=c(1,2,1,1), data=c(NA,3,NA,1))
#' coalesce_join(df1,df2,by=c('id','id2'))
coalesce_join <- function(x,
                          y,
                          by = NULL,
                          suffix = c(".x", ".y"),
                          join = dplyr::left_join,
                          ...) {

  x <- x[,!str_count(names(x), ".x|.y") >= 1]
  y <- y[,!str_count(names(y), ".x|.y") >= 1]

  joined <- join(x, y, by = by, suffix = suffix, ...)
  joined <- joined[,!str_count(names(joined), ".x|.y") >= 2]

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
#'
#' @param list
#' @param by
#' @param join
#' @param ...
#' @export

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
#'
#' @param df
#' @param threshold
#' @param na_as_a_cat
#' @param exclude
#' @export
#' @example
#' rm_single_unique_col(data.frame(a = c("a", "a", "b"), b = c(NA, NA, NA), c=c(1,1,1)), exclude = "b")

rm_single_unique_col <- function (df,
                                  threshold = 1,
                                  na_as_a_cat = TRUE,
                                  exclude = c())
{
  if (length(df) <= 1 | !is.data.frame(df)) {
    return(df)
  }

  df <- data.table::as.data.table(df)

  ids_excl <- which(names(df) %in% exclude)
  ids <- ids_excl

  df1 <- df[, ids, with = FALSE]
  rest <- setdiff(seq_along(df), ids)
  df2 <- df[, rest, with = FALSE]
  df2 <- df2[,!sapply(df2, function(x) {
    all(is.na(x))
  }), with = FALSE]

  if (na_as_a_cat == FALSE) {
    uniq <- sapply(df2, function(x) {
      length(unique(x[!is.na(x)]))
    })
  }

  if (na_as_a_cat == TRUE) {
    uniq <- sapply(df2, function(x) {
      length(unique(x))
    })
  }


  df3 <- cbind(df1, df2[,!uniq %in% c(0:threshold), with = FALSE])
  return(df3)
}


#' Remove row(s) if all values are NAs
#'
#' @param df
#' @export
#' @example
#' remove_empty_rows(data.frame(a=c(1,NA,2), b=c("A",NA,NA)))

remove_empty_rows <- function(df) {
  if (!is.data.frame(df))
    stop("not a data frame")
  df %>% filter(Reduce(`+`, lapply(., is.na)) != ncol(.))
}

#' Remove duplicated rows by leaving only 1 observation per each group of most non-missing data columns
#' if multiple observations sharing equal number of most non-missing data columns, choose the first one by row index
#' a wrapper function of d8ahelper::any_dups
#'
#' @param df
#' @param keys a character vector contains column names, for grouping purposes
#' @export
#'
#' @example
#' rm_dups_w_less_data(data.frame(a=c("a", "a", "b", "b"), b=c(NA,2,3,3)), "a")

rm_dups_w_less_data <- function(df, keys) {
  dt <- data.table::as.data.table(df)

  dups <- any_dups(dt, keys = keys)

  dt.uniq <- dt[!dups$dup_row_bool, ]

  dt.dup <- dups$dups

  not_missing <-
    sapply(dt.dup[, -keys, with = FALSE], function(col) {
      if (!mode(col) %in% c("character", "factor")) {
        return(!(is.na(as.numeric(col)) | as.numeric(col) == ""))
      }

      return(!(is.na(col) | col == ""))
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
#' @export
#' @example
#' insert_nas(mtcars, percent = 0.1)

insert_nas <- function(df, percent = 0.2) {
  as.data.frame(lapply(df, function(x) {
    "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, percent))))
  }))
}



#' Remove NAs given a vector
#'
#' @param x
#' @export
#' @example
#' rm_na(c(1,2,NA,3))
rm_na <- function(x) {
  x[!is.na(x)]
}

#' Remove duplicated rows of the original data frame, or a subset of if column names being passed in
#'
#' @param df
#' @param ... additional column names to be used for usbset by columns
#'
#' @return
#' @export
#'
#' @examples
#' @example
#' remove_duplicates(data.frame(a=c(1,2,3,4,5,2), b=c(1,2,5,4,3,2), c=c(2,2,1,4,5,2)),c("b","c"))
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

#' Taken a list of data frames, check each column and remove duplicated ones from each dataframe if they have already been identified in LHS data frames in the list
#'
#' @param df_list a list of data.frame objects
#' @param exclude names of columns that will be left alone from being removed
#'
#' @return
#' @export
#'
#' @example
#' rm_dup_cols_in_a_list(list(a=data.frame(a=1,b=2,c=3), b=data.frame(a=2,b=3,d=1)))
rm_dup_cols_in_a_list <- function(df_list, exclude=c()) {
  all_col_names <- Reduce(c, sapply(df_list, names))

  occ_times <-
    sapply(all_col_names, function(x)
      sum(x == all_col_names))

  col_more_than_one <- unique(all_col_names[occ_times > 1])

  #reference for if a column has been preserved and ready to be removed if there are more
  col_kept <- character()

  new_list_of_df <- list()

  for (n in seq_along(df_list)) {
    df <- df_list[[n]]

    matched_cols <- intersect(names(df), col_more_than_one)

    #leave alone id cols
    matched_cols <- setdiff(matched_cols, exclude)

    if (length(matched_cols) > 0) {
      for (col in matched_cols) {
        if (col %in% col_kept) {
          df[[col]] <- NULL
          print(glue::glue("data.frame #{n}: {col} removed"))
        } else {
          col_kept <- append(col_kept, col)
        }
      }
    }

    new_list_of_df[[n]] <- df
  }

  return(new_list_of_df)
}


#' Identify NAs and empty cells that fit a pattern and replace
#'
#' @param df
#' @param pat
#' @param fill
#'
#' @return
#' @export
#'
#' @example
#' na_as_missing(data.frame(a=c(NA,"","  ", "-","NA", "missing","only row without missing")))
na_as_missing <- function (df,
                                pat = "^(\\s*\\[*missing\\]*\\s*|\\s*|NA|\\s*-\\s*)$",
                                fill = "-")
{
  if (!is.data.frame(df)) {
    message("not a dataframe, return original")
    return(df)
  }

  pb <- progress::progress_bar$new(total = ncol(df))
  list_of_cols <-
    d8ahelper::lapply_preserve_names(df, function(col) {
      # if(is.list(col)){col_tmp <- col[[1]]}
      if (is.character(col[[1]]) | is.factor(col[[1]])) {
        col[[1]][is.na(col[[1]]) | grepl(pat, col[[1]])] <- fill
      }
      pb$tick()
      return(as.data.frame(col))
    })
  return(do.call(cbind, list_of_cols))
}

#' Sister func to na_as_missing, replace cells with NAs if match to given string
#'
#' @param df
#' @param pattern
#' @export
#' @example
#' missing_as_na(data.frame(a=c(NA, NA, "NA", "")))
#' missing_as_na(fill_na_as_missing(data.frame(a=c(NA, NA, "NA", ""),b=c(2, 1, "", " "))))

missing_as_na <- function(df, pattern = "-") {
  df[df == pattern] <- NA
  df
}

#' Identify missing pattern or 'NA' and replace with NAs
#'
#' @param df
#' @param fill
#' @param pat
#'
#' @return
#' @export
#'
#' @example
#' fill_missing_as_na(fill_na_as_missing(data.frame(a=c(NA, NA, "NA", ""),b=c(2, 1, "", " "))))
fill_missing_as_na <- function(df,
                               fill = NA,
                               pat = "^(\\s*\\[*missing\\]*\\s*|\\s*|NA|\\s*-\\s*)$") {
  if (!is.data.frame(df)) {
    message("not a dataframe, return original")
    return(df)
  }
  list_of_cols <-
    d8ahelper::lapply_preserve_names(df, function(col) {
      # if(is.list(col)){col_tmp <- col[[1]]}
      if (is.character(col[[1]]) | is.factor(col[[1]])) {
        col[[1]][grepl(pat, col[[1]])] <- fill
      }
      return(as.data.frame(col))
    })
  return(do.call(cbind, list_of_cols))
}


#' Artificially increase the number of rows to make up any missing combinations by label columns for every unique id
#' 'Puff up' the dataframe by filling in the missing entries with artificial data, default NA
#' Will move id columns and label columns to the far left
#' tidyr::crossing and tidyr::expand.grid perform similar function
#'
#' @param df
#' @param id_col
#' @param label_col
#' @param fill_with
#' @export
#' @example
#' puff_my_df(data.frame(id=c("a","b","d","e"), id2=c(1,2,2,2), label=c(1,2,3,4), data=c(1,2,3,4)), id_col="id", label_col= "id2", "missing")

puff_my_df <- function(df, id_col, label_col, fill_with = NA) {
  df <- as.data.frame(df)

  any_missing <- Reduce("|", lapply(df[id_col], is.na))
  df[any_missing,] <- NULL

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

    upper <- df[df[[new_id]] == x,]
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

#' Remove rows where id columns are all missing, and rows where all columns but the id columns are missing; (optional) Remove columns where all rows are of missing values.
#'
#' @param df
#' @param id_col numeric vector specify which columns are considered id columns
#' @param var_col numeric vector specify which columns are considered data columns
#' @param filter_col a bool, if TRUE then will leave only columns that does not contain missing - applies to all non id columns if var_col is NULL otherwise only applies to var_col only.
#' @param th a numeric, range from 0-1 to specify the threshold for amount least amount of non-missing data by column in fractions of total rows
#'
#' @return
#' @export
#'
#' @examples
clean_by_id <-
  function(df,
           id_col,
           var_col = NULL,
           filter_col = FALSE,
           th = 1) {
    dt <- data.table::as.data.table(df)

    #row operation by id cols
    row_no_ids <-
      rowSums(is.na(dt[, id_col, with = FALSE])) == length(id_col)

    if (any(row_no_ids)) {
      dt <- dt[-row_no_ids,]
    }

    if (is.null(var_col)) {
      rhs <- dt[, -id_col, with = FALSE]
    }

    if (!is.null(var_col)) {
      rhs <- dt[, var_col, with = FALSE]
    }

    #row operation to rhs
    row_all_na <- apply(rhs, 1, function(x) {
      all(is.na(x))
    })

    dt <- dt[!row_all_na, ]

    if (filter_col == FALSE) {
      rhs <- rhs[!row_all_na, ]
    }

    #column operation to rhs
    if (filter_col == TRUE) {
      #rhs may be different since entire-empty rows were removed
      if (is.null(var_col)) {
        rhs <- dt[, -id_col, with = FALSE]
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

#' find columns that has at minimum 'th' number of non-NA values and fill missings with upper above closest non-NA value
#'
#' @param df
#' @param th threshold value used to identify columns with minimal number of unique value count, if min value < th then add to target column list
#' @param direction
#'
#' @return
#' @export
#'
#' @example
#' fill_cols(data.frame(a=c(1, NA, 1, NA), b=c(1,1, 1, NA), b=c(1, 1, NA, NA)), th = 2)

fill_cols <- function(df, th = 1, direction = "down") {
  vals <- names(df)[sapply(df, function(x)
    sum(!is.na(x)) <= th)]
  df <- dplyr::as_tibble(df)
  df <- tidyr::fill(df, one_of(vals), .direction = "down")
  return(df)
}

# encode and decode column names --------------------------------------------------------------
#' Replace column names with alpha-numeric sequences, returns a named vector (of name-value pairs) for column name look-up; Returns a list.
#'
#' @param df
#' @export
#' @example
#' l <- encode_col(mtcars)

encode_col <- function(df) {
  dict <- names(df)
  names(dict) <- paste0("c", seq_along(df))

  names(df) <- paste0("c", seq_along(df))

  return(list("tag" = dict,
              "df" = df))

}

#' Sister func to encode_col
#' returns a data frame with original column names based on df$dict
#'
#' @param df.list: list object generated from d8ahelper::convert_col_name
#' @param df: data frame to convert names from, default to df within df.list
#' @export
#' @example
#' l <- decode_col(encode_col(mtcars))
decode_col <- function(df.list, df = df.list$df) {
  names(df) <- sapply(names(df), function(x)
    df.list$tag[[x]])
  return(df)
}

#' get names of encoded obj, sister func to encode_col, retrieve column names based on ids
#'
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' get_name(encode_col(mtcars),c('c1','c3'))
get_name <- function(df, ...) {
  sapply(..., function(x)
    df$tag[[x]])
}

#' get id from encoded obj, sister func to encode_col, retrieve column id based on names
#'
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' get_id(encode_col(mtcars),'mpg')
#' get_id(encode_col(mtcars),c('mpg','disp'))
get_id <- function(df, ...) {
  sapply(..., function(x)
    names(df$tag)[df$tag == x])
}
