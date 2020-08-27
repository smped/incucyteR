#' @title Summarise Treatments from an IncucyteExperiment object
#' @description Provide numbers of replicates from treatment groups
#' @details
#' This summarises any columns from the requested slot which are not positional
#' (i.e. row, column etc) and summarises the number of values found for each
#' (treatment) group found
#' @param x An IncucyteExperiment object
#' @param by Summarise by columns in the colData or plateMap slots
#' @return A grouped tibble
#' @importFrom dplyr select group_by_all tally
#' @importFrom tidyselect any_of
#' @rdname summariseBy-methods
#' @export
setGeneric(
    name = "summariseBy",
    def = function(x, by = c("colData", "plateMap")){
        standardGeneric("summariseBy")
    }
)
#' @rdname summariseBy-methods
#' @export
setMethod("summariseBy", signature = "ANY", function(x, by){
    .errNotImp(x)
}
)
#' @rdname summariseBy-methods
#' @export
setMethod(
    f = "summariseBy",
    signature = "IncucyteExperiment",
    definition = function(x, by = c("colData", "plateMap")){

        rmCols <- c("image", "well", "nesting", "row", "col")
        by <- match.arg(by)

        if (by == "colData"){
            df <- colData(x)
        }
        if (by == "plateMap"){
            df <- metadata(x)$plateMap$map
            if (nrow(df) == 0) {
                msg <- "No PlateMap data was found. Unable to summarise"
                message(msg)
                return(invisible(NULL))
            }
        }

        df <- as.data.frame(df)
        df <- dplyr::select(df, -any_of(rmCols))
        df <- dplyr::group_by_all(df)
        dplyr::tally(df)

    }
)
