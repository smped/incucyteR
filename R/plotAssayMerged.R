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
#' @param point_aes Plotting aesthetics for points.
#' Any aesthetics must refer to columns in the colData slot of x.
#' Any aesthetics provided here will become the grouping categories for
#' calculating and plotting the mean+se of each category.
#' Must be specified.
#' @param line_aes Plotting aesthetics for lines.
#' If not provided will default to the same as point_aes
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
        x, assay, point_aes, line_aes, trans = "log10", pointsize = 1/3
    ){
        standardGeneric("plotAssayMerged")
    }
)
#' @rdname plotAssayMerged-methods
#' @export
setMethod("plotAssayMerged", signature = "ANY", function(x){
    .errNotImp(x)
}
)
#' @rdname plotAssayMerged-methods
#' @export
setMethod(
    f = "plotAssayMerged",
    signature = "IncucyteExperiment",
    definition = function(
        x, assay, point_aes, line_aes, trans = "log10", pointsize = 1/3
    ){
        ## Check the aesthetics
        if (missing(point_aes)) {
            msg <- paste0(
                "Plotting aesthetics must be supplied for points.\n",
                "These must correspond to columns in the colData slot."
            )
            stop(msg)
        }
        if (missing(line_aes)) line_aes <- point_aes
        stopifnot(.checkAes(point_aes, x))
        stopifnot(.checkAes(line_aes, x))
        ## Check the assays
        if (length(assay) > 1) {
            msg <- "Too many assays. Only the first will be plotted."
            warning(msg)
            assay <- assay[[1]]
        }
        ## Make the plot
        df <- assay2long(x, assay)
        ## Maybe we should place the dots here for other aesthetics & have
        ## them all inherited
        ggplot(df, aes_string("Elapsed", assay)) +
            stat_summary(
                point_aes,
                fun.data = mean_se, geom = "pointrange",
                size = pointsize
            ) +
            stat_summary(line_aes, fun = mean, geom = "line") +
            scale_y_continuous(trans = trans)

    }
)
