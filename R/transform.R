#' Convinient functions for most frequent data transformation needs.

filter_by_quantile <- function(df,
                               col,
                               top = 0,
                               bottom = 0,
                               ...){
  #' remove top and(or) bottom x%
  #' return data.frame(tibble) or numeric vector based on input
  #' col must be string format(with "")

  if(is.data.frame(df)){
  df %>%
    filter(df[[col]] >= quantile(df[[col]], bottom, na.rm=TRUE),
           df[[col]] <= quantile(df[[col]], 1-top, na.rm=TRUE),
           )
  }else{
      upper = quantile(df, bottom, na.rm=TRUE)
      lower = quantile(df, 1-top, na.rm=TRUE)
      df[df >= upper & df <= lower]
  }
}
