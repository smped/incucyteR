#' @title The IncucyteExperiment Class
#'
#' @description The IncucyteExperiment Class
#'
#' @details
#' The IncucyteExperiment class extends the SummarizedExperiment class and is
#' an S4 class
#'
#' @param f A vector or list of file paths containing the observed Incuvyte
#' data
#' @param map File path to the PlateMap file
#'
#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
.IncucyteExpt <- setClass("IncucyteExperiment", contains="SummarizedExperiment")

#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom S4Vectors DataFrame
#' @importFrom forcats fct_inorder
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
IncucyteExperiment <- function(f, map) {

    ## For each file in f, parse the cell counts and define as a matrix
    ## Pass any names in f as the assayNames
    nm <- names(f)
    if (length(nm) == 0) nm <- paste0("assay", seq_along(f))
    if (any(nm == ""))
        stop("All names for f must be either blank, or provided")
    names(f) <- nm
    vals <- lapply(f, .parseIncucyte)
    assayHdr <- lapply(vals, function(x){x[["hdr"]]}) ## For the metadata
    vals <- lapply(vals, function(x){x[["mat"]]})
    ## Ensure identical row & column names across assays
    stopifnot(.checkAssays(vals))

    ## rowData will simply be the time parsed, along with Elapsed & scaledTime
    assayRows <- rownames(vals[[1]])
    rd <- DataFrame(
        Time = parse_date_time(assayRows, orders = "%d/%m/%Y %I:%M:%S %p"),
        row.names = assayRows
    )
    rd$Elapsed <- as.numeric(rd$Time - min(rd$Time)) / 3600 ## In hours
    rd$ElapsedScaled <- rd$Elapsed / max(rd$Elapsed)

    ## colData can be extracted from each column name
    ## The 'anchors' should be the wells and images
    assayCols <- colnames(vals[[1]])
    image <- gsub(".+Image ([0-9])$", "\\1", assayCols)
    well <- gsub(".+well \\(([A-Z][0-9]{1,2})\\).+", "\\1", assayCols)
    plateRow <- gsub("[0-9]*", "", well)
    plateCol <- gsub("[A-Z]", "", well)
    cd <- DataFrame(
        image, well,
        nesting = paste(well, image, sep = "_"),
        row = plateRow, col = as.integer(plateCol),
        row.names = assayCols
    )

    ## Merge with plateMap data if provided
    mapData <- list(hdr = NULL, map = NULL, .attrs = NULL)
    if (!missing(map)){
        mapData <- .parsePlateMap(map)
        cd <- left_join(as.data.frame(cd), mapData$map, by = c("row", "col"))
        cd <- DataFrame(cd)
        rownames(cd) <- assayCols
    }

    ## Set everything as factors after merging with the plateMap
    cd$image <- as.factor(cd$image)
    cd$well <- fct_inorder(cd$well)
    cd$nesting <- fct_inorder(cd$nesting)
    cd$row <- as.factor(cd$row)
    cd$col <- as.factor(cd$col)

    ## Form the basic Summarized Experiment & add the Incucyte class
    se <- SummarizedExperiment(
        assays = vals,
        colData = cd,
        rowData = rd,
        metadata = list(
            assay = assayHdr,
            map = list(
                hdr = DataFrame(mapData$hdr),
                map = DataFrame(mapData$map)
            )
        )
    )
    .IncucyteExpt(se)
}
