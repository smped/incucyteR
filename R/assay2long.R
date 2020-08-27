#' @title Convert assayData to long form
#'
#' @description Convert assayData to long form with additional colData
#'
#' @details
#' Extract each requested assay from an IncucyteExperiment and return a
#' data.frame in long form including each time-point and sample name.
#' By default any metadata in the colData slot will be added to the output
#'
#' @param x An IncucyteExperiment object
#' @param assay One or more of the assays contained in the IncucyteExperiment
#' object. Defaults to all assays
#' @param addColData logical. Include colData in the output
#' @param ... not used
#' @return A data.frame
#'
#' @importFrom purrr reduce
#' @importFrom dplyr left_join
#' @importFrom tibble rownames_to_column
#' @importFrom SummarizedExperiment assay assayNames colData rowData
#'
#' @name assay2long
#' @rdname assay2long-methods
#' @export
setGeneric(
    name = "assay2long",
    def = function(x, assay, addColData = TRUE, ...){
        standardGeneric("assay2long")
    }
)
#' @rdname assay2long-methods
#' @export
setMethod("assay2long", signature = "ANY", function(x, ...){
    .errNotImp(x)
}
)
#' @rdname assay2long-methods
#' @export
setMethod(
    f = "assay2long",
    signature = "IncucyteExperiment",
    definition = function(x, assay, addColData = TRUE){

        ## Default to all
        if (missing(assay)) assay <- assayNames(x)
        if(!all(assay %in% assayNames(x))) stop("Incorrect assayName provided")
        dfs <- lapply(assay, function(y){
            vals <- assay(x, y)
            df <- data.frame(
                Time = rep(rownames(vals), times = ncol(vals)),
                sample = rep(colnames(vals), each = nrow(vals)),
                assay = as.numeric(vals)
            )
            names(df) <- gsub("assay", y, names(df))
            df
        })
        out <- purrr::reduce(dfs, left_join, by = c("Time", "sample"))
        if (addColData){
            cd <- colData(x)
            cd <- as.data.frame(cd)
            cd <- tibble::rownames_to_column(cd, "sample")
            out <- left_join(out, cd, by = "sample")
        }
        out <- cbind(rowData(x)[out[["Time"]],], out[setdiff(colnames(out), "Time")])
        rownames(out) <- NULL
        as.data.frame(out)
    }

)
