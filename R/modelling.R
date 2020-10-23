


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



# unsupervised learnings --------------------------------------------------

#'Hierchical clustering


#' Hiearchical clustering with for within cluster SSs screeplot for easier cluster number selection
#' wrapper function of 'fastcluster' package method hclust (for matrix) and hclust.vector(for vector)
#'
#' @param x an (N×D) matrix of 'double' values: N observations in D variables.
#' @param method the agglomeration method to be used. This must be (an unambiguous abbreviation of) one of "single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid" or "median" (for matrices) or "single", "ward", "centroid" or "median" (for a vector)

hclust_wss <- function(x,
                       method = "ward",
                       max_clust = 10,
                       members = NULL,
                       visual = TRUE) {
  if (is.vector(x) == TRUE) {
    clust <- fastcluster::hclust.vector(freq, method = method)
  } else if(is.matrix(x) == TRUE) {
    dist <- dist(x)
    clust <-
      fastcluster::hclust(dist, method = method, members = members)
  } else {
    stop("x must be a vector or matrix")
  }

  if (max_clust >= length(x)) {
    max_clust = length(x)
  }

  rank <- sapply(seq_len(max_clust), function(i) {
    cl <- cutree(clust, i)
    spl <- split(x, cl)
    sum(sapply(spl, function(d) {
      #wss calculation
      sum(scale(d, scale = FALSE) ^ 2)
    }))
  })

  if (visual == TRUE) {
    #screeplot
    plot(seq_along(rank), rank, type = "b", pch = 19, xlab = "num of clusters", ylab = "wss")
  }

  return(clust)
}




#' a wrapper function to 'gstat::gstat' function to use a univariate or multivariate geostatistical model with input of x,y coordinate and fill z column with predictions, for each id group.
#' puff_my_df can be used to make all available entries available for each group

fill_map <- function(df, idcol, xcol='x', ycol='y', zcol='z'){

  if (length(idcol) > 1){

    combined_id_col <- Reduce(function(x,y){paste(x, y, sep="_")}, df[, idcol])
    new_id <- paste(idcol, collapse="_")
    df[[new_id]] <- combined_id_col
  } else {
    new_id <- idcol
  }

  unique_id <- unique(df[[new_id]])

  if(is.null(df[[zcol]])){stop("are you sure you had the correct z column name?")}

  df <- move_left(df, c(xcol,ycol,zcol))
  names(df)[1:3] <- c("x", "y", "z")

  pb <- progress::progress_bar$new(total = length(unique_id))

  df_list <- lapply(unique_id, function(id){

    pb$tick()

    subset_rows <- df[[new_id]]==id

    df <- df[subset_rows, ]

    is_empty_z <- is.na(df$z)

    if (!any(is_empty_z)){
      return(df)
    }

    train <- df[!is_empty_z, ]
    pred <- df[is_empty_z, ]

    gs_model <- gstat::gstat(formula=z~1, locations = ~x+y, data=train)
    p <- predict(gs_model, pred)$var1.pred

    df$z[is_empty_z] <- p

    return(df)
  })

  result <- do.call(rbind, df_list)

  names(result)[1:3] <- c(xcol, ycol, zcol)

  if (length(idcol) > 1) {
    result[[new_id]] <- NULL
  }

  return(result)
}
