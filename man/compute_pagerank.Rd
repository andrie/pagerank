% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_pagerank.R
\name{compute_pagerank}
\alias{compute_pagerank}
\title{Computes pagerank for CRAN packages.}
\usage{
compute_pagerank(mirror, decreasing = TRUE)
}
\arguments{
\item{mirror}{CRAN mirror, e.g. http://cran.revolutionanalytics.com or https://mran.revolutionanalytics.com/snapshot/2016-03-15}

\item{decreasing}{If TRUE, sorts the result in decreasing order}
}
\description{
Computes pagerank for CRAN packages.
}
\examples{
MRAN <- "https://mran.revolutionanalytics.com/snapshot/2016-03-15/"
pr <- compute_pagerank(MRAN)
head(pr, 10)

plot_graph(MRAN, 10)
plot_graph(MRAN, 10, cex = 0.8)
plot_graph(MRAN, 25)
plot_graph(MRAN, 50)
}

