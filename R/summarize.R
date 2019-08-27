#' Convinient functions for most frequently used data summarizing needs.

sum_table <- function(df, x, y, sum = TRUE, ...) {
  if (!is.character(c(x, y))) {
    print("requires column names as character strings in quotes")
  } else{
    table <- table(df[[x]], df[[y]])
    if (sum == TRUE) {
      table1 <- cbind(table, Total = rowSums(table))
      table2 <- rbind(table1, Total = colSums(table1))
      return(table2)
    } else{
      return(table)
    }
  }
}

sum_col <- function(df,
                    outlier_method = "z",
                    thres = 3,
                    ...) {
  # returns a dataframe that summarize the input data table
  if (!is.data.frame(df)) {
    print("requires input of dataframe or tibble")
  } else{
    data.frame(
      "name" = colnames(df),
      "class" = sapply(df, class),
      "type" = sapply(df, typeof),
      "unique.val" = sapply(df, function(x) {
        length(unique(x))
      }),
      "non.missing" = sapply(df, function(x) {
        length(x) - sum(is.na(x))
      }),
      "non.missing.pct" = sapply(df, function(x) {
        percent <- ((length(x) - sum(is.na(x))) * 100 / length(x))
        as.double(format(round(percent, 2), nsmall = 2))
      }),
      "examples(min|1Q|Med(Mean)|3Q|max)" = sapply(df, function(x) {
        if (is.numeric(x)) {
          paste(
            as.double(format(round(
              min(x, na.rm = TRUE), 1
            ), nsmall = 1)),
            " | ",
            as.double(format(round(
              quantile(x, 0.25, na.rm = TRUE), 1
            ), nsmall = 1)),
            " | ",
            as.double(format(round(
              median(x, na.rm = TRUE), 1
            ), nsmall = 1)),
            "(",
            as.double(format(round(
              mean(x, na.rm = TRUE), 1
            ), nsmall = 1)),
            ") | ",
            as.double(format(round(
              quantile(x, 0.75, na.rm = TRUE), 1
            ), nsmall = 1)),
            " | ",
            as.double(format(round(
              max(x, na.rm = TRUE), 1
            ), nsmall = 1))
          )
        } else{
          paste(unique(x[!is, .na(x)])[1:min(10, length(unique(x)))], collapse = " | ")
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
}

sum_missing <- function(df, top_n = 5, ...) {
  #returns a tibble that summarize top (n) missing value-column combinations

  df[!is.na(df)] <- 0
  df[is.na(df)] <- 1

  df_foo <- dplyr::as_tibble(sapply(df, as.character)) %>%
    unite("id", colnames(.), sep = "") %>%
    group_by(id) %>%
    summarize(n = n()) %>%
    arrange(-n) %>%
    top_n(top_n) %>%
    rowwise() %>%
    mutate(
      "position" =
        paste0(which(strsplit(id, "")[[1]] == "1"),
               collapse = ","),
      "colname" = paste0(names(df)[which(strsplit(id, "")[[1]] == "1")],
                         collapse = ",")
    )
}

