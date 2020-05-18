


# pre-processing ------------------------------------------------------------------------------


corr_to_df <- function(cor,
                       level = 0.9 #TODO: figure out proper th
                       ) {
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
           level = 0.99, #TODO: figure out proper th
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
      result <- d8ahelper::corr_to_df(descrCor, level = 1)

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

    cor <- d8ahelper::corr_to_df(descrCor.df[, -highlyCorDescr], level = level)
    cor.removed <-
      d8ahelper::corr_to_df(descrCor.df[, highlyCorDescr], level = level)

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
