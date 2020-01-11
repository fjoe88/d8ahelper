


# Summarize ---------------------------------------------------------------------------------

headtail <- function(df, n = 5) {
  #' combine head and tail to show first and last n rows of dataframe

  if (nrow(df) <= 2*n) {
    head(df, nrow(df))
  } else {
    skip.row <- as.data.frame(
      matrix(data = "...", ncol = ncol(df))
    )
    row.names(skip.row) <- glue::glue("(skip {nrow(df)-2*n} rows)")
    names(skip.row) <- names(df)

    rbind(head(df, n), skip.row, tail(df, n))
  }
}

sum_table <- function(df, x, y, sum = TRUE, ...) {
  # output a data.frame table with total count of each unique x by y (column)

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

sum_col <- function(df,
                    outlier_method = "z",
                    thres = 3,
                    summary_split = FALSE,
                    ...) {
  # returns a dataframe that summarize the input data table

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
      percent <- ((length(x) - sum(is.na(x))) * 100 / length(x))
      as.double(format(round(percent, 2), nsmall = 2))
    }),
    "examples(mean)" = sapply(df, function(x) {
      if (!is.numeric(x)) {
        unique(x[!is.na(x)])[1]

      } else{
        as.double(format(round(mean(x, na.rm = TRUE), 1), nsmall = 1))
      }
    }),
    "summary(min|1Q|med|3Q|max)" = sapply(df, function(x) {
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

sum_missing <- function(df, num = 5, ...) {
  #give top n missing value column combinations

  df[!is.na(df)] <- 0
  df[is.na(df)] <- 1

  df_foo <- as_tibble(sapply(df, as.character)) %>%
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
}
