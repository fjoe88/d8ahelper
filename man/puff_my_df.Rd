% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{puff_my_df}
\alias{puff_my_df}
\title{Artificially increase the number of rows to make up any missing combinations by label columns for every unique id
'Puff up' the dataframe by filling in the missing entries with artificial data, default NA
Will move id columns and label columns to the far left
tidyr::crossing and tidyr::expand.grid perform similar function}
\usage{
puff_my_df(df, id_col, label_col, fill_with = NA)
}
\description{
Artificially increase the number of rows to make up any missing combinations by label columns for every unique id
'Puff up' the dataframe by filling in the missing entries with artificial data, default NA
Will move id columns and label columns to the far left
tidyr::crossing and tidyr::expand.grid perform similar function
}
