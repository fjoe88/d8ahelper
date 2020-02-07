


# pre-processing ------------------------------------------------------------------------------

filter_col_unique <- function(df,
                              percentUnique = 50,
                              freqRatio = 1,
                              breaks = 50,
                              useDefault = FALSE,
                              plot = TRUE,
                              simplify = FALSE) {
  #' wraper function to caret::nearZeroVar to filter chr columns for Zero- and Near Zero-Variance Predictors
  #'@param df a data frame
  #'@param percentUnique a numeric for percentUnique threshold
  #'@param freqRatio a numeric for freqRatio threshold
  #'@param useDefault a logical for if to use caret default for removing near zero variance variables
  #'@param plot a logical for if to include histograms of percentUnique and freqRatio
  #'@param simplify a logical for if to simplify output to a single data frame, else return a list

  nzv <- caret::nearZeroVar(df, saveMetrics = TRUE)

  if (plot == TRUE) {
    hist(nzv$freqRatio, breaks = breaks)
    hist(nzv$percentUnique, breaks = breaks)
  }

  if (useDefault == TRUE) {
    # default
    nzv <- nearZeroVar(df_data)
    return(df[, -nzv])
  }

  df1 <-
    df[, nzv$percentUnique < percentUnique & nzv$freqRatio < freqRatio * nrow(df)]

  df2 <- df[, !(nzv$percentUnique < percentUnique & nzv$freqRatio < freqRatio * nrow(df))]


  if (simplify == TRUE) {return(df1)
  } else {
    list("df" = df1, "df.filtered" = df2)
  }
}

create_dummy <- function(df,
                         response,
                         simplify = FALSE,
                         filter_chr = TRUE,
                         ...) {
  #'wraper function to use caret::dummyVars to create dummy variables from character variables
  #'@param df a data frame
  #'@param response response columns, if any
  #'@param simplify a logical for if to simplify output to a dataframe, if FALSE returns a list
  #'@param filter_chr a logical for if calling filter_col_unique for removing columns based on near zero variance predictors

  df <- wafer::format_all(df)

  col_id <- wafer::detect_all(df, return_col_names = TRUE)
  col.c <- names(df)[sapply(df, is.character)]
  col.n <- names(df)[sapply(df, is.numeric)]
  col.f <- names(df)[sapply(df, is.factor)]

  df.id <- df[col_id]
  df.rsps <- df[response]
  df.c <- df[setdiff(col.c, c(col_id, response))]
  df.n <- df[setdiff(col.n, c(col_id, response))]
  df.f <- df[setdiff(col.f, c(col_id, response))]

  if (filter_chr==TRUE){
    df.c <- filter_col_unique(df.c, simplify = TRUE)
  }

  # fullRank for total dependent vars (1 or the other scenario)
  dmy <- caret::dummyVars( ~ ., data = df.c, fullRank = TRUE)
  df.c.dummified <- as.data.frame(predict(dmy, newdata = df.c))
  names(df.c.dummified) <- str_remove(names(df.c.dummified), "^`")
  names(df.c.dummified) <- str_replace_all(names(df.c.dummified), "`", "...")

  if (simplify == TRUE){
    return(cbind(df.id, df.rsps, df.f, df.n, df.c.dummified))
  } else (
    return(list("df.id"  = df.id,
                "df.rsps" = df.rsps,
                "df.fct" = df.f,
                "df.num" = df.n,
                "df.chr" = df.c,
                "df.chr.dummy" = df.c.dummified))
  )
}

corr_to_df <- function(cor,
                       level = 0.9) {
  #' filter high corr features and output in tidy format
  #' input: corr matrix of numerical format
  #' output: a data frame
  #' if level == 1, find perfectly correlated features aside from themselves

  cor <- abs(cor)
  cor[upper.tri(cor)] <- NA
  cor <- as.data.frame(cor)

  df1 <- lapply_preserve_names(cor, function(col) {
    data.frame(
      "x" = names(col),
      "indx" = which(col >= level),
      "corr" = rm_na(col[col >= level])
    )
  })

  df2 <- lapply(df1, function(df) {
    df$y = names(cor)[df$indx]
    df <- df[, !(names(df) == "indx")]
    df <- d8ahelper::move_left(df, "corr")
  })

  df3 <- do.call(rbind, df2)

  #remove self-self
  df3 <- df3[df3$x != df3$y,]

  return(df3)
}


remove_high_corr_features <-
  function(df,
           level = 0.99,
           simplify = FALSE) {
    #' wraper function ofcaret::findCorrelation: find and remove highly correlated features
    #'
    #' @param df a data frame of numerical values w/o NAs
    #' @param level a numeric for threshold as cutoff point for defining 'high correlation'
    #' returns a list of 1) reduced df 2) corr table of removed features
    #' requires data frame of numeric w/o missing values
    #' @details if level == 1, find all perfectly correlated variables, remove half and return a list containing removed feature name pairs, unless simplify == TRUE
    #'
    descrCor <- cor(df)
    descrCor.df <- as.data.frame(descrCor)

    if (level == 1) {
      result <- corr_to_df(descrCor, level = 1)

      col.to.remove <- which(names(df) %in% result$y)
      df <- df[, -col.to.remove]

      if (simplify == TRUE) {
        return(df)
      } else{
        return(list("df" = df,
                    "cor.removed.features" = result))
      }

    }

    # on 1-hot decoded colomns, need to be careful for removing highly corr columns
    highlyCorDescr <-
      caret::findCorrelation(descrCor, cutoff = level) # col.id to remove

    cor <- corr_to_df(descrCor.df[, -highlyCorDescr], level = level)
    cor.removed <-
      corr_to_df(descrCor.df[, highlyCorDescr], level = level)

    if (simplify == TRUE) {
      descrCor.df[, -highlyCorDescr]
    } else{
      list(
        "df" = descrCor.df[, -highlyCorDescr],
        "cor.df" = cor,
        "cor.removed.features" = cor.removed
      )
    }

  }

# Modeling ------------------------------------------------------------------------------------

model_lm <- function(formula, dataset) {
  #' run lm model and print a curated summary output
  model1 <- lm(formula = formula, data = dataset)
  stats <- round(
    c(
      summary(model1)$fstatistic[c(1, 3)],
      summary(model1)$sigma,
      summary(model1)$r.squared,
      summary(model1)$adj.r.squared
    ),
    3
  )
  names(stats) <- c("F", "DF", "Sigma", "Rsq", "AdjRsq")
  l1 <- list(round(summary(model1)$coefficients, 3), stats)
  names(l1) <- c("Coefficients", "Stats")
  print(l1)

  #run specific diagnostic tests
  par(mfrow = c(1, 3))
  hist(model1$residuals, main = "Histogram of residuals", xlab = "")
  plot(model1, 1)
  plot(model1, 2)
}
