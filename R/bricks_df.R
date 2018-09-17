#' @title Bricks \code{data.frame} function
#'
#' @name bricks
#'
#' @description Creates a new \code{bricks} object used to make coverages.
#' A \code{bricks} object contains four mandatory attributes: 'file', 'key', 'band', and 'timeline'.
#' The 'key' and 'band' attributes are extracted from file name.
#' A bricks 'key' is a name that identifies all bands files of one brick. The bricks keys
#' text are trimmed on the following default characters \code{getOption("trim_keys", "_. ")}.
#' A bricks 'band' is the name of the band.
#'
#' @param files      A \code{character} vector listing all raster files location to be used as bricks.
#' @param timeline   A \code{date} vector indicating all dates of each brick time series.
#' @param bands      A \code{data.frame} containing 'name', 'short_name', 'min', 'max',
#'                   'fill', and 'scale' columns for each band to be retrieved from \code{files}.
#'
#' @return A bricks \code{data.frame} object.
#'
#' @export
#'
bricks <- function(files, timeline, bands = coverage::MOD13Q1_bands) {

    index <- match(.files.bands(files, bands = bands), bands[["band_long_name"]])

    files <- files[!is.na(index)]
    index <- index[!is.na(index)]

    if (.is_empty(index)) {

        stop("No file bands information matched the bands defition.")
    }

    message("Retrieving brick(s) metadata...")

    metadata <- .files.metadata(files)

    message("Done.")

    bricks <- tibble::as_tibble(cbind(tibble::as_tibble(list(
        file     = files,
        key      = .files.keys(files = files, bands = bands))),
        metadata,
        bands[index,]
    ))

    bricks[["timeline"]] <- list(as.character(unlist(timeline, use.names = FALSE)))

    .check_bricks_df(bricks)

    return(bricks)
}

#' @title Internal bricks \code{data.frame} functions
#'
#' @name .check_bricks_df
#'
#' @description Check if the bricks \code{data.frame} attribute values are consistent and
#' all files are reachable.
#' A progress bar will be showed if the number of files in bricks is greater or equal
#' the value of \code{getOption("progress_bar", 10)}.
#'
#' @param bricks         A bricks \code{data.frame}.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_bricks_df <- function(bricks) {

    if (.is_empty(bricks)) {

        stop("Informed bricks is empty.")
    }

    if (.is_empty(bricks[["file"]])) {

        stop("The bricks definition has one or more empty 'file' values.")
    }

    if (.is_empty(bricks[["key"]])) {

        stop("The bricks definition has one or more empty 'key' values.")
    }

    if (any(bricks[["time_len"]] != length(bricks[["timeline"]][[1]]))) {

        warning(sprintf(paste(
            "One or more bricks 'timeline' have inconsistent length (%s)",
            "with fetched 'time_len' metadata (%s).",
            length(bricks[["timeline"]][[1]]), bricks[["time_len"]]
        )), call. = FALSE)
    }

    tryCatch(
        .as_lom(bricks, .template.bricks),
        error = function(e) {

            stop("Invalid brick data.")
        }
    )

    return(TRUE)
}
