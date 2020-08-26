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

    ## Collect the header information
    hdr <- ln[seq_len(dataStarts - 1)]

    list(hdr = hdr, mat = mat)

}

#' @title Parsing function for Incucyte PlateMap
#' @description Parsing function for Incucyte PlateMap
#' @param f File path for an Incucyte PlateMap file
#' @return numeric matrix
#' @keywords internal
#' @importFrom XML xmlParse xmlToList
#' @importFrom dplyr bind_rows arrange
.parsePlateMap <- function(f, value_sep = "/", .autoGroup = TRUE){

    ## Import without modification
    stopifnot(file.exists(f))
    plateMap <- xmlParse(f)
    plateMap <- xmlToList(plateMap)

    ## Find all of the possible metadata fields
    refItems <- plateMap$referenceItemManager$referenceItems

    ## Find the wells with metaData
    allWells <- plateMap$wellStore$wells
    hasMeta <- vapply(
        allWells,
        function(x){"items" %in% names(x)},
        logical(1)
    )

    ## The wells with metadata (i.e. not empty)
    map <- lapply(allWells[hasMeta], .getWellItem, sep = value_sep)
    map <- do.call("rbind", map)

    ## Add the empty wells
    empty <- do.call("rbind", allWells[!hasMeta])
    map <- bind_rows(
        as.data.frame(map), as.data.frame(empty)
    )
    rownames(map) <- NULL

    ## Rows & columns may be zero-based, so shift to 1-based
    map[c("row", "col")] <- lapply(
        X = map[c("row", "col")],
        FUN = function(x){
            x <- as.integer(x)
            x <- x + (min(x) == 0)
            x
        }
    )
    map$row <- LETTERS[map$row]

    hdr <- do.call("rbind", plateMap$referenceItemManager$referenceItems)
    rownames(hdr) <- NULL

    ## If automatically creating groups
    if (.autoGroup){
        map$Group <- apply(
            X = map[unique(hdr[,"type"])],
            MARGIN = 1,
            FUN = paste,
            collapse = value_sep
        )
    }

    list(
        hdr = hdr,
        map = dplyr::arrange(map, row, col),
        .attrs = plateMap$.attrs
    )

}

#' @title Extract well metadata
#' @description Extract well metadata from an XML PlateMap
#' @details
#' This collates & reorganises data from within each well in the metadata
#' @param x A single well extracted from the complete PlateMap
#' @keywords internal
.getWellItem <- function(x, sep = "/"){
    ## Check that there is metadata
    n <- length(x$items)
    stopifnot(all(n > 0))
    ## Collect the values
    vals <- lapply(
        seq_len(n),
        function(i){
            x$items[[i]]$referenceItem
        }
    )
    vals <- do.call("rbind", vals)
    ## Remove colours & the description.
    ## We can add the description in the larger metadata object
    vals <- vals[, !grepl("(color|description)", colnames(vals))]
    valList <- split(vals, f = vals[,"type"])
    valList <- lapply(
        X = valList,
        FUN = function(x){
            mat <- matrix(x, ncol = ncol(vals))
            colnames(mat) <- colnames(vals)
            vec <- apply(mat, 2, function(y){paste(unique(y), collapse = sep)})
            vec[setdiff(names(vec), "type")]
        }
    )
    out <- unlist(valList)
    out <- c(out, row = x$.attrs[["row"]], col = x$.attrs[["col"]])
    names(out) <- gsub("\\.displayName", "", names(out))
    out

}
