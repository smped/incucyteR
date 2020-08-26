#' @title Check assays
#' @description Check the rownames and colnames for multiple assays
#' @param v list of values after importing with .parseIncucyte
#' @return logical(1)
#' @keywords internal
.checkAssays <- function(l){

    ## Only perform checks if more than one element
    n <- length(l)
    if (n == 1) return(TRUE)

    ## Check identical column names
    refCols <- colnames(l[[1]])
    chkCols <- vapply(l, function(x){all(colnames(x) == refCols)}, logical(1))
    if (!all(chkCols)) stop("Assay column names do not match")

    ## Check for identical rownames
    refRows <- rownames(l[[1]])
    chkRows <- vapply(l, function(x){all(rownames(x) == refRows)}, logical(1))
    if (!all(chkRows)) stop("Assay row names do not match")
    TRUE
}

#' @title Check supplied aesthetics are valid for the IncucyteExperiment
#' @param aes A ggplot aesthetic
#' @param x The IncucyteExperiment object to be plotted
#' @return logical(1)
#' @keywords internal
.checkAes <- function(aes, x){

    is_aes <- all(
        uneval = is(aes, "uneval"),
        vapply(aes, is, logical(1), class2 = "quosure")
    )
    cols <- vapply(
        X = aes,
        FUN = function(x){attributes(terms(x))$term.labels},
        FUN.VALUE = character(1)
    )
    in_data <- all(cols %in% colnames(colData(x)))
    return(all(is_aes, in_data))
}
