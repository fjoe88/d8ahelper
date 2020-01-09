

# Transform -----------------------------------------------------------------------------------

trim_spaces <- function(x,
                        leading = FALSE,
                        trailing = FALSE,
                        all = FALSE,
                        replacement = "_") {
  #'trim leading / trailing by default
  #'trim in between white spaces if 'all == TRUE'
  #'reduce white spaces to single separaters specified with 'replacement = '

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

move_left <- function(df, str) {
  #' take col name or RE and move them to far left
  #' accept multiple column names passed in as character vectors

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

coalesce_join <- function(x,
                          y,
                          by = NULL,
                          suffix = c(".x", ".y"),
                          join = dplyr::left_join,
                          ...) {
  #join x, y data frames by appending y values to x if missing

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


# Not yet commited

add_empty_rows <- function(df, n) {
  # append empty rows to a dataframe to make row number = n

  new.row <- rep(NA, length = ncol(df))
  new.row <- rbind(new.row)
  m <- length(df[[1]])
  to.append <-
    as.data.frame(new.row[rep(1:nrow(new.row), length = n - m), , drop = FALSE])
  names(to.append) <- names(df)
  new.df <- rbind(df, to.append, use.names = FALSE)
}

convert_time_to_chr <- function(df, regex = "Time") {
  #' background: POSIXct datetime format when output as csv will not be read corretly with JMP
  #' purpose: convert all time columns to character format befor exporting to csv
  #' note: accept regex if default rule of selecting columns containing "time" is not ideal

  df %>% dplyr::mutate_at(vars(contains(!!regex)), as.character)
}


rm_single_unique_col <- function(df) {
  #'remove columns if contain single unique value

  foo <- sapply(df, function(x) {
    length(unique(x))
  }) # output a named integer vector

  col_removed <- paste(names(foo)[foo == 1], collapse = ", ")

  message(glue(
    "Columns removed bacause of containing single unique value: \n {col_removed}"
  ))

  df.out <- df[, !foo == 1]
}
