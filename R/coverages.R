#' @title Coverage functions
#'
#' @name coverage
#'
#' @description Create a new \code{coverage} object. A \code{coverage} object
#' contains all information about the coverage authoring, data policy, and
#' bricks definitions.
#'
#' @param bricks     A bricks \code{data.frame}.
#' @param manifest   A description \code{list} returned by \code{coverage.manifest} function.
#'
#' @return A \code{coverage} object.
#'
#' @export
#'
coverage <- function(bricks, manifest = manifest()) {

    .check_bricks_df(bricks)

    coverage <- structure(list(
        manifest = .as_lom(.as_df(manifest), .template.manifest),
        bands    = .as_lom(bricks, .template.bands),
        timeline = .as_lom(bricks[, "timeline"], .template.timeline),
        bricks   = .as_lom(bricks, .template.bricks)
    ), class = "coverage")

    return(coverage)
}

#' @title Coverage functions
#'
#' @name check_coverage
#'
#' @description Checks the coverage validity.
#'
#' @param coverage      A \code{coverage} object.
#' @param touch_files   Verify if raster files exists.
#'
#' @return The \code{coverage} object with \code{class} attribute if pass in all checks.
#'
#' @export
#'
check_coverage <- function(coverage, touch_files = TRUE) {

    .check_bricks(coverage[["bricks"]])

    # Checking if all brick files exists
    if (touch_files) {

        message("Checking if all files are reachable...")

        .files.touch(files = .as_df(coverage[["bricks"]])[["file"]])

        message("Done.")
    }

    return(coverage)
}

#' @title Coverage functions
#'
#' @name print.coverage
#'
#' @description Prints a readable summary for \code{coverage} object.
#'
#' @param x     A \code{coverage} object to be printed on screen.
#' @param ...   Other parameters passed by \code{print} generic function.
#'
#' @export
#'
print.coverage <- function(x, ...) {

    cat(sprintf("Description: %s\n", x[["manifest"]][["description"]]))
    cat(sprintf("Bands: %s\n",
                paste(.as_df(x[["bands"]])[["band_short_name"]],
                      collapse = ", ")))
    cat("\n")
    cat("Bricks table")
    print(get_bricks(x))
}

#' @title Internal coverage functions
#'
#' @name .check_coverage
#'
#' @description Checks the coverage validity.
#'
#' @param coverage   A \code{coverage} object.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_coverage <- function(coverage) {

    if (class(coverage) == "character") {

        stop("Invalid coverage object.")
    }

    .check_manifest(coverage[["manifest"]])
    .check_bands(coverage[["bands"]])
    .check_coverage_timeline(coverage)
    .check_bricks(coverage[["bricks"]])

    return(coverage)
}

#' @title Internal coverage functions
#'
#' @name .check_bands
#'
#' @description Check for validity of a coverages bands definition.
#'
#' @param bands   A bands definition \code{list} object.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_bands <- function(bands) {

    if (.is_empty(bands)) {

        stop("The coverage bands definition is empty.")
    }

    tryCatch(.as_lom(.as_df(bands), .template.bands),
             error = function(e) {

                 stop("Invalid coverage bands definition data.")
             })

    return(TRUE)
}

#' @title Internal coverage functions
#'
#' @name .check_coverage_timeline
#'
#' @description Check for validity of a coverage timeline field.
#'
#' @param coverage   A \code{coverage} object.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_coverage_timeline <- function(coverage) {

    if (.is_empty(coverage)) {

        stop("The coverage bands definition is empty.")
    }

    if (any(coverage[["bricks"]][["time_len"]] != length(coverage[["timeline"]][[1]]))) {

        warning(sprintf(paste(
            "One or more bricks 'time_len' metadata are inconsistent with",
            "'timeline' length (%s)",
            length(coverage[["timeline"]][[1]]))), call. = FALSE)
    }

    return(TRUE)
}

#' @title Internal coverage functions
#'
#' @name .check_bricks
#'
#' @description Check for validity of a coverages bricks list.
#'
#' @param bricks        A bricks \code{list} object.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_bricks <- function(bricks) {

    if (.is_empty(bricks)) {

        stop("The coverage bricks list is empty.")
    }

    tryCatch(.as_lom(.as_df(bricks), .template.bricks),
             error = function(e) {

                 stop(sprintf("Invalid coverage bricks data with error: \"%s\".",
                              e$message))
             })

    return(TRUE)
}

#' @title Coverage functions
#'
#' @name get_bricks
#'
#' @description Joins bricks with bands information from \code{coverage} object.
#' The values can be nested in lists by each key.
#'
#' @param coverage      A \code{coverage} object.
#' @param nest_by_key   A \code{logical} indicating if values must be nested by key.
#'
#' @return A bricks \code{data.frame} object.
#'
#' @export
#'
get_bricks <- function(coverage, nest_by_key = FALSE) {

    .check_coverage(coverage)

    bands <- .as_df(coverage[["bands"]])
    bricks <- .as_df(coverage[["bricks"]])
    index <- match(bricks[["band_long_name"]], bands[["band_long_name"]])

    if (nest_by_key) {

        nested <- unique(bricks[,"key"])
        for (i in names(bricks)) {

            nested[[i]] <- unlist(tapply(bricks[[i]], bricks[["key"]], function(x) {
                value <- unique(x)
                if (length(value) > 1) list(x) else value
            }, simplify = FALSE), recursive = FALSE, use.names = FALSE)
        }
        for (i in names(bands)) {

            nested[[i]] <- unlist(tapply(
                bands[[i]][index], bricks[["key"]], list, simplify = FALSE),
                recursive = FALSE, use.names = FALSE)
        }

        return(nested)
    }

    for (i in names(bands)) {

        bricks[[i]] <- bands[[i]][index]
    }

    return(bricks)
}
