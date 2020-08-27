#' @title Add cumulative values to the assays
#'
#' @description Add assays with cumulative values to an IncucyteExperiment
#'
#' @details
#' After specifying which assays, this calculates a cumulative sum of the
#' values in each requested assay. This may be useful as assays which measure
#' death are inherently transient, whilst those which measure growth are
#' implicitly cumulative
#'
#' @param x An IncucyteExpeiment object
#' @param assay The assays for which a cumulative sum is required.
#' Defaults to all existing assays
#' @param name Names to give to the new (cumulative) assays
#' @param allowDecrease logical vector. Allows the cumulative sum to decrease.
#' For counts of dead cells, allowing a decrease may not be appropriate as dead
#' cells cannot revive. However, for growth assays this may be more relevant
#' @param suffix If the \code{name} argument is not provided, this suffix will
#' be added to the original assay name
#' @param sep Text separator use to separate the assay name and the suffix
#'
#' @return An IncucyteExperiment object
#'
#' @importFrom SummarizedExperiment assayNames assays assay assays<-
#'
#' @name addCumulative
#' @rdname addCumulative-methods
#' @export
setGeneric(
    name = "addCumulative",
    def = function(
        x, assay, name, allowDecrease = FALSE, suffix = "cumulative", sep = "_"
    ){
        standardGeneric("addCumulative")
    }
)
#' @rdname addCumulative-methods
#' @export
setMethod(
    f = "addCumulative",
    signature = "ANY",
    definition = function(
        x, assay, name, allowDecrease = FALSE, suffix = "cumulative", sep = "_"
    ){
        .errNotImp(x)
    }
)
#' @rdname addCumulative-methods
#' @export
setMethod(
    f = "addCumulative",
    signature = "IncucyteExperiment",
    definition = function(x, assay, name, allowDecrease = FALSE, suffix = "cumulative", sep = "_"){

        if (missing(assay)) assay <- assayNames(x)
        stopifnot(all(assay %in% assayNames(x)))
        n <- length(assay)

        if (missing(name)) name <- paste(assay, suffix, sep = sep)
        stopifnot(length(name) == n)
        if (length(allowDecrease) == 1) allowDecrease <- rep(allowDecrease, n)

        for (i in seq_len(n)){
            new <- apply(
                X = assay(x, assay[[i]]),
                MARGIN = 2,
                FUN = function(x){
                    vals <- c(x[[1]], diff(x))
                    if (!allowDecrease[[i]]) vals[vals < 0] <- 0
                    cumsum(vals)
                }
            )
            rownames(new) <- rownames(assay(x, assay[[i]]))
            assays(x)[[name[[i]]]] <- new
        }
        x

    }
)
