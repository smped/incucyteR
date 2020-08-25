#' @title The IncucyteExperiment Class
#'
#' @description The IncucyteExperiment Class
#'
#' @details
#' The IncucyteExperiment class extends the SummarizedExperiment class and is
#' an S4 class
#'
#' @param f A vector or list of file paths. Names will be used as assayNames
#' and are highly recommended
#'
#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
.IncucyteExpt <- setClass("IncucyteExperiment", contains="SummarizedExperiment")

#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment
IncucyteExperiment <- function(f, ...) {

    ## For each file in f, parse the cell counts and define as a matrix
    ## Pass any names in f as the assayNames
    vals <- lapply(f, .parseIncucyte)

    se <- SummarizedExperiment(list(counts=counts), ...)
    .IncucyteExpt(se)
}
