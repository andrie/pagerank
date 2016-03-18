#' @importFrom miniCRAN makeDepGraph
#' @importFrom igraph page.rank
#' @importFrom memoise memoise
#' @importFrom utils available.packages
NULL


makeDepGraph <- function(pdb, pkg = pdb[, "Package"]){
  miniCRAN::makeDepGraph(pkg,
               availPkgs = pdb,
               suggests=FALSE,
               enhances=TRUE,
               includeBasePkgs = FALSE)
}

getPackages <- function(mirror){
  available.packages(
    contrib.url(mirror, type = "source"),
    type="source",
    filters = NULL)  
}

getGraph <- memoise(makeDepGraph)

#' Computes pagerank for CRAN packages.
#'
#' @param mirror CRAN mirror, e.g. http://cran.revolutionanalytics.com or https://mran.revolutionanalytics.com/snapshot/2016-03-15
#' 
#' @param decreasing If TRUE, sorts the result in decreasing order
#'
#' @example /inst/examples/examples_compute_pagerank.R
#' @export
compute_pagerank <- function(mirror, decreasing = TRUE){
  pdb <- getPackages(mirror)
  g <- getGraph(pdb)
  pr <- page.rank(g, directed = FALSE)$vector
  if(decreasing) sort(pr, decreasing = TRUE) else pr
}

#' Plots the dependency graph
#' 
#' @inheritParams compute_pagerank
#' @param top integer that indicates how many packages to include in the graph
#' @param cex character expansion ratio for plot
#' @param random.seed A random seed value. Used by igraph to initialise the plot layout. The function resets the seed to the original value, i.e. it will not have any impact on the random value stream in your R session.
#' 
#' @export
plot_graph <- function(mirror, top = 25, cex = 0.5, random.seed = 1){
  pdb <- getPackages(mirror)
  g <- getGraph(pdb)
  pr <- compute_pagerank(mirror, decreasing = TRUE)
  pr <- names(head(pr, top))
  pdb <- getGraph(pdb, pr)
  old.seed <- .Random.seed
  on.exit(.Random.seed <- old.seed)
  set.seed(random.seed)
  plot(pdb, main = sprintf("Top %s packages by page rank", length(pr)), cex = cex)
}
