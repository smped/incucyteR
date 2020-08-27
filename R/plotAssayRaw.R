#' @title Plot the raw assayData
#' @description Plot the raw assayData from an IncucyteExperiment object
#' @details
#' Visualise all data from the requested assay. By default a line plot will
#' be produced for every image within each well. The plot will also be broken
#' into facets using the row and column information.
#'
#' Lines can be coloured by any additional column contained in the colData slot
#' of the IncucyteExperiment object by setting the argument
#' \code{colour = colName}, which will be passed to \code{aes_string()} within
#' the initial call to \code{ggplot}. Any additional columns must be quoted.
#'
#' Additional information can also be added to the facet headers by adding
#' the column you wish. For example, if each column contains a unique CellType,
#' you could specify \code{facet = facet_grid(row~col+CellType)}. Note that any
#' additional column must also be contained in the colData slot.
#'
#' The returned object is a ggplot object and can be further modified using
#' scales, themes and additional geoms.
#'
#' @param x An IncucyteExperiment object
#' @param assay The assay to be visualised
#' @param ... Passed to \code{aes_string()} in the initial call to
#' \code{ggplot}. Any aesthetics can be specified here, with values passed to
#' the aesthetics as a character, and that match a column name from colData.
#' @param trans The transformation for the y-axis
#' @param facet Faceting to add to the plot
#'
#' @return A ggplot object
#' @importFrom ggplot2 facet_grid ggplot aes_string geom_line aes scale_y_continuous
#' @export
#'
#' @name plotAssayRaw
#' @rdname plotAssayRaw-methods
#' @export
setGeneric(
    name = "plotAssayRaw",
    def = function(
        x, assay, ..., trans = "log10", facet = facet_grid(row~col)
    ){
        standardGeneric("plotAssayRaw")
    }
)
#' @rdname plotAssayRaw-methods
#' @export
setMethod("plotAssayRaw", signature = "ANY", function(x, ...){
    .errNotImp(x)
}
)
#' @rdname plotAssayRaw-methods
#' @export
setMethod(
    f = "plotAssayRaw",
    signature = "IncucyteExperiment",
    definition = function(
        x, assay, ..., trans = "log10", facet = facet_grid(row~col)
    ){

        if (length(assay) > 1) {
            msg <- "Too many assays. Only the first will be plotted."
            warning(msg)
            assay <- assay[[1]]
        }
        df <- assay2long(x, assay)
        image <- c() # Avoids R CMD error
        ggplot(df, aes_string(x = "Elapsed", y = assay, ...)) +
            geom_line(aes(group = image)) +
            facet +
            scale_y_continuous(trans = trans)
    }
)
