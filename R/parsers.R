#' @title Parsing function for Incucyte output
#' @description Parsing function for Incucyte output
#' @param f File path for an Incucyte output file
#' @return numeric matrix
#' @keywords internal
.parseIncucyte <- function(f){

    ## Basic checks
    stopifnot(file.exists(f))

    ## Find where the data starts
    ln <- readLines(f)
    dataStarts <- grep("Date Time", ln)[[1]]
    stopifnot(length(dataStarts) == 1) # Stop if "Date Time" isn't found
    dataEnds <- length(ln)

    ## Setup as a matrix/data.frame
    rw <- seq(dataStarts + 1, dataEnds, by = 1)
    splitVals <- strsplit(ln[rw], split = "\t")
    mat <- do.call("rbind", splitVals)
    colnames(mat) <- strsplit(ln[dataStarts], "\t")[[1]]
    rownames(mat) <- mat[,"Date Time"]
    mat <- mat[, setdiff(colnames(mat), c("Date Time", "Elapsed"))]
    class(mat) <- "numeric"

    mat

}

#' @title Parsing function for Incucyte PlateMap
#' @description Parsing function for Incucyte PlateMap
#' @param f File path for an Incucyte PlateMap file
#' @return numeric matrix
#' @keywords internal
.parsePlateMap <- function(f){

    ## Basic checks
    stopifnot(file.exists(f))


}
