#' @title Plot the assayData merged across replicate wells
#' @description Plot the assayData from an IncucyteExperiment object merged
#' across replicate wells
#' @details
#' Visualise all data from the requested assay merged by the column(s) passed
#' to the line and point aesthetics
#'
#'
#' The returned object is a ggplot object and can be further modified using
#' scales, themes and additional geoms.
#'
#' @param x An IncucyteExperiment object
#' @param assay The assay to be visualised
#' @param ... Passed to \code{aes_string()}. If no colour, linetype or other
#' grouping category is provided, the plot will simply be a summary of all
#' points in all wells. Any provided categories must correspond to columns
#' contained in the colData slot of \code{x}
#' @param trans The transformation for the y-axis
#' @param pointsize The size of points to be drawn
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes_string stat_summary scale_y_continuous mean_se
#' @export
#'
#' @name plotAssayMerged
#' @rdname plotAssayMerged-methods
#' @export
setGeneric(
    name = "plotAssayMerged",
    def = function(
        x, assay, ..., trans = "log10", pointsize = 1/3
    ){
        standardGeneric("plotAssayMerged")
    }
)
#' @rdname plotAssayMerged-methods
#' @export
setMethod("plotAssayMerged", signature = "ANY", function(x, ...){
    .errNotImp(x)
}
)
#' @rdname plotAssayMerged-methods
#' @export
setMethod(
    f = "plotAssayMerged",
    signature = "IncucyteExperiment",
    definition = function(
        x, assay, ..., trans = "log10", pointsize = 1/3
    ){

        ## Check the assays
        if (length(assay) > 1) {
            msg <- "Too many assays. Only the first will be plotted."
            warning(msg)
            assay <- assay[[1]]
        }

        ## Make the plot
        df <- assay2long(x, assay)
        ggplot(df, aes_string("Elapsed", assay, ...)) +
            stat_summary(
                fun.data = mean_se, geom = "pointrange",
                size = pointsize
            ) +
            stat_summary(fun = mean, geom = "line") +
            scale_y_continuous(trans = trans)

    }
)
