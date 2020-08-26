#' @title Visualise the Incucyte PlateMap
#'
#' @description Visualise the Incucyte PlateMap from an IncucyteExperiment
#'
#' @details
#' This will produce a visualisation of the PlateMap if supplied in the
#' IncucyteExperiment object
#'
#' @param x An IncucyteExperiment objecy
#' @param fill PlateMap Column used to fill tiles on the plot
#' @param gridCol Colour for gridlines
#' @param gridSize Thickness of gridlines
#' @param naCol Colour for empty wells, or wells in the PlateMap with no data
#' @param ... Passed to scale_fill_discrete
#'
#' @return A standard ggplot2 object
#'
#' @docType methods
#'
#' @import ggplot2
#' @importFrom forcats fct_rev
#'
#' @name plotPlateMap
#' @rdname plotPlateMap-methods
#' @export
setGeneric(
    name = "plotPlateMap",
    def = function(
        x, fill = "Group", gridCol = "grey70", gridSize = 0.25,
        naCol = "white", ...
    ){
        standardGeneric("plotPlateMap")
    }
)
#' @rdname plotPlateMap-methods
#' @export
setMethod("plotPlateMap", signature = "ANY", function(x, ...){
    .errNotImp(x)
}
)
setMethod(
    f = "plotPlateMap",
    signature = "IncucyteExperiment",
    definition = function(
        x, fill = "Group", gridCol = "grey70", gridSize = 0.25,
        naCol = "white", ...
    ){
        pm <- metadata(x)$plateMap$map
        if (nrow(pm) == 0) {
            msg <- "No PlateMap was supplied for this Incucyte Experiment"
            warning(msg)
            return(invisible(NULL))
        }
        pm <- as.data.frame(pm)
        if (!fill %in% colnames(pm)) stop(
            paste("Could not find", fill, "in supplied PlateMap")
        )
        stopifnot(all(c("row", "col") %in% colnames(pm)))
        pm$row <- as.factor(pm$row)
        pm$row <- fct_rev(pm$row)
        pm$col <- as.factor(pm$col)

        ## Now produce the plot
        ggplot(pm, aes_string("col", "row", fill = fill)) +
            geom_tile() +
            geom_vline(
                xintercept = seq_along(levels(pm$col)) + 0.5,
                colour = gridCol,
                size = gridSize
            ) +
            geom_hline(
                yintercept = seq_along(levels(pm$row)) + 0.5,
                colour = gridCol,
                size = gridSize
            ) +
            scale_x_discrete(expand = expansion(0, 0)) +
            scale_y_discrete(expand = expansion(0, 0)) +
            scale_fill_discrete(na.value = naCol, ...) +
            theme(
                axis.line = element_line(colour = gridCol, size = gridSize),
                panel.grid = element_blank()
            )
    }
)
