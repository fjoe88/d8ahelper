
# Summarize ---------------------------------------------------------------------------------

#' A quick glance into a dataframe combining n head and tail rows
#'
#' @param n number of top/bottom rows to include, or number of characters in case of a string
#' @return top, bottom and a transition rows suggest how many rows skipped
#' @examples
#' headtail(mtcars)
#' headtail("I would like to make this a very long string in order to make my case")

headtail <- function(df, n = 5) {
  if (is.data.frame(df)) {
    if (nrow(df) <= 2 * n) {
      head(df, nrow(df))
    } else {
      skip.row <- as.data.frame(matrix(data = "...", ncol = ncol(df)))
      row.names(skip.row) <-
        glue::glue("(skip {nrow(df)-2*n} rows)")
      names(skip.row) <- names(df)

      rbind(head(df, n), skip.row, tail(df, n))
    }
  } else if (is.character(df)) {
    sapply(df, function(x) {
      if (nchar(x) <= 2 * n) {
        x
      } else {
        head <- stringr::str_extract(x, paste0("^(.){", n, "}"))
        tail <- stringr::str_extract(x, paste0("(.){", n, "}$"))
        paste(head, tail, sep = " ... ")
      }
    })

  }
}

#' get mode of a vector
#'
#' @example
#' x <- c("a","d","a","b","a","c",NA,NA,NA,NA,NA)
#' get_mode(x)
#' get_mode(x, na.rm = FALSE)

get_mode <- function(x, na.rm = TRUE) {
  if (na.rm == TRUE) {
    ux <- x[!is.na(x)]
  } else {
    ux <- unique(x)
  }

  ux[which.max(tabulate(match(x, ux)))]
}


#' function factory for nth quantile

ff_quantile <- function(num){
  function(x, ...) {
    quantile(x, num, ...)
  }
}

q5  <- ff_quantile(0.05)
q10 <- ff_quantile(0.10)
q25 <- ff_quantile(0.25)
q50 <- ff_quantile(0.50)
q75 <- ff_quantile(0.75)
q90 <- ff_quantile(0.90)
q95 <- ff_quantile(0.95)


#' Extract unique rows combinations and show row counts

unique_row <- function(df, ...) {
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
    df5 <- df5[order(df5$count, decreasing = TRUE),]

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
    df5 <- df5[order(df5$count, decreasing = TRUE),]
  }
}

#' Given 2 columns, summarize counts of each unique value-pair combinations
#'
#' @param x a column #, unique values displayed as row headers
#' @param y a column #, unique values displayed as column headers
#' @return a data frame
#' @example
#' sum_table(ToothGrowth, x = "supp", y = "dose")

sum_table <- function(df, x, y, sum = TRUE, ...) {

  if (!is.character(x) &&
      is.character(y))
    stop("only accept character x & y")

  table <- table(df[[x]], df[[y]])
  if (sum == TRUE) {
    table1 <- cbind(table, Total = rowSums(table))
    table2 <- rbind(table1, Total = colSums(table1))
    return(table2)

  } else {
    return(table)
  }
}

#' Summarize a data frame with concise and useful summary statistics
#'
#' @param df a data frame
#' @param outlier_method "z" (Normal) or "mad" (Nonparametric) for how to categorize outliers
#' @param thres a number to suggest number of standard deviations to use for categorize outliers
#' @return a data frame
#' @example
#' View(sum_col(iris))

sum_col <- function(df,
                    outlier_method = "z",
                    thres = 3,
                    ...) {

  if (!is.data.frame(df))
    stop("Only data.frame object is accepted")

  print(class(df))

  summary <- dplyr::tibble(
    "col.name" = colnames(df),
    "col.class" = sapply(df, class),
    "col.type" = sapply(df, typeof),
    "unique.val" = sapply(df, function(x) {
      length(unique(x))
    }),
    "non.missing" = sapply(df, function(x) {
      length(x) - sum(is.na(x))
    }),
    "non.missing.pct" = sapply(df, function(x) {
      percent <- ((length(x) - sum(is.na(x))) / length(x))
      d8ahelper::format_to_percentage(percent)
      }),
    "mean" = sapply(df, function(x) {
      if (!is.numeric(x)) {
        # unique(x[!is.na(x)])[1]
        NA
      } else{
        as.double(format(round(mean(x, na.rm = TRUE), 1), nsmall = 1))
      }
    }),
    "summary(min|1Q|med|3Q|max(if num) or examples(if chr))" = sapply(df, function(x) {
      if (!is.numeric(x)) {
        paste(unique(x[!is.na(x)])[1:min(10, length(unique(x)))], collapse = " | ")
      } else{
        paste(
          as.double(format(round(
            min(x, na.rm = TRUE), 1
          ), nsmall = 1)),
          as.double(format(round(
            quantile(x, 0.25, na.rm = TRUE), 1
          ), nsmall = 1)),
          as.double(format(round(
            median(x, na.rm = TRUE), 1
          ), nsmall = 1)),
          as.double(format(round(
            quantile(x, 0.75, na.rm = TRUE), 1
          ), nsmall = 1)),
          as.double(format(round(
            max(x, na.rm = TRUE), 1
          ), nsmall = 1)),
          sep = "|"
        )
      }
    }),
    "ol.low" = sapply(df, function(x) {
      if (is.numeric(x)) {
        if (outlier_method == "z") {
          low = mean(x, na.rm = TRUE) - thres * sd(x, na.rm = TRUE)
          as.double(format(round(low, 2), nsmall = 1))
        } else{
          if (outlier_method == "mad") {
            low = median(x, na.rm = TRUE) - thres * mad(x, na.rm = TRUE)
            as.double(format(round(low, 2), nsmall = 1))
          }
        }
      }
    }),
    "ol.high" = sapply(df, function(x) {
      if (is.numeric(x)) {
        if (outlier_method == "z") {
          low = mean(x, na.rm = TRUE) + thres * sd(x, na.rm = TRUE)
          as.double(format(round(low, 2), nsmall = 1))
        } else{
          if (outlier_method == "mad") {
            low = median(x, na.rm = TRUE) + thres * mad(x, na.rm = TRUE)
            as.double(format(round(low, 2), nsmall = 1))
          }
        }
      }
    })
  )
}

sum_row <- function(df) {
  #'generate row-wise summary for missing and unique values
  #'
  data.frame(
    "NAs" = apply(df, 1, function(x)
      sum(is.na(x))),
    "NA.Pct" = apply(df, 1, function(x)
      d8ahelper::format_to_percentage(sum(is.na(x)) / length(x))),
    "NA.Position" = apply(df, 1, function(x)
      d8ahelper::headtail(paste(
        which(is.na(x)), collapse = ","
      ), n = 15)),
    "UNIQs" = apply(df, 1, function(x)
      length(unique(x)))
  )
}

sum_df <- function(df){
  #'sister function to sum_col and sum_row
  #'returns a list
  #'
  list("col_summary" <- d8ahelper::sum_col(df),
       "row_summary" <- d8ahelper::sum_row(df))
}

#' Summary statistics of top n missing value columns
#'
#' @param df a data frame
#' @param num top n number of missing rows to return
#' @example
#' foo <- d8ahelper::insert_nas(mtcars) #generate missing values for each row
#' View(sum_missing(foo))

sum_missing <- function(df, num = 5, ...) {

  df[!is.na(df)] <- 0
  df[is.na(df)] <- 1

  df_missing <- as_tibble(sapply(df, as.character)) %>%
    unite("id", colnames(.), sep = "") %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    arrange(-n) %>%
    top_n(num) %>%
    rowwise() %>%
    mutate(
      "position" = paste0(which(strsplit(id, "")[[1]] == "1"), collapse = ","),
      "colname" = paste0(names(df)[which(strsplit(id, "")[[1]] == "1")], collapse = ",")
    )

  df_missing <- move_left(df_missing, "colname")
}


