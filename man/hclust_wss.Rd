% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling.R
\name{hclust_wss}
\alias{hclust_wss}
\title{Hiearchical clustering with for within cluster SSs screeplot for easier cluster number selection
wrapper function of 'fastcluster' package method hclust (for matrix) and hclust.vector(for vector)}
\usage{
hclust_wss(x, method = "ward", max_clust = 10, members = NULL, visual = TRUE)
}
\arguments{
\item{x}{an (N×D) matrix of 'double' values: N observations in D variables.}

\item{method}{the agglomeration method to be used. This must be (an unambiguous abbreviation of) one of "single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid" or "median" (for matrices) or "single", "ward", "centroid" or "median" (for a vector)}
}
\description{
Hiearchical clustering with for within cluster SSs screeplot for easier cluster number selection
wrapper function of 'fastcluster' package method hclust (for matrix) and hclust.vector(for vector)
}
