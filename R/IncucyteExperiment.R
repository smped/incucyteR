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
#' @importFrom forcats fct_inorder
#' @importFrom lubridate parse_date_time
IncucyteExperiment <- function(f, map) {

    ## For each file in f, parse the cell counts and define as a matrix
    ## Pass any names in f as the assayNames
    nm <- names(f)
    if (length(nm) == 0) nm <- paste0("assay", seq_along(f))
    if (any(nm == ""))
        stop("All names for f must be either blank, or provided")
    names(f) <- nm
    vals <- lapply(f, .parseIncucyte)
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
        plateRow, plateCol,
        row.names = assayCols
    )

    ##############################
    ## Merge with plateMap data ##
    ##############################

    ## Set everything as factors after merging with the plateMap
    ## There may be missing wells where no sample was run if we do this prior
    cd$image <- as.factor(cd$image)
    cd$well <- fct_inorder(cd$well)
    cd$nesting <- fct_inorder(cd$nesting)
    cd$plateRow <- as.factor(cd$plateRow)
    cd$plateCol <- as.factor(cd$plateCol)

    ## Form the basic Summarized Experiment & add the Incucyte class
    se <- SummarizedExperiment(vals, colData = cd, rowData = rd)
    .IncucyteExpt(se)
}
