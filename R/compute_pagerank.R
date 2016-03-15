#' @importFrom miniCRAN makeDepGraph
#' @importFrom igraph page.rank
NULL


#' Computes pagerank for CRAN packages.
#'
#' @param mirror CRAN mirror, e.g. http://cran.revolutionanalytics.com or https://mran.revolutionanalytics.com/snapshot/2016-03-15
#'
#' @example /inst/examples/examples_compute_pagerank.R
#' @export
compute_pagerank <- function(mirror, decreasing = TRUE){

  pdb <- available.packages(
    contrib.url(mirror, type = "source"),
    type="source",
    filters = NULL)

    # Note that this step takes a while, expect ~15-30 seconds

  g <-  makeDepGraph(pdb[, "Package"],
                     availPkgs = pdb,
                     suggests=FALSE,
                     enhances=TRUE,
                     includeBasePkgs = FALSE)

  # Use the page.rank algorithm in igraph -----------------------------------

  pr <- page.rank(g, directed = FALSE)$vector
  if(decreasing) sort(pr, decreasing = TRUE) else pr
}

