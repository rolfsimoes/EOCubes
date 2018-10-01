#' @title Coverage functions
#'
#' @name coverage
#'
#' @description Create a new \code{coverage} object. A \code{coverage} object
#' contains all information about the coverage authoring, data policy, and
#' bricks definitions.
#'
#' @param bricks_df   A bricks \code{data.frame}.
#' @param timeline    A \code{date} vector indicating all dates of each brick time series.
#' @param manifest    A description \code{list} returned by \code{coverage.manifest} function.
#'
#' @return A \code{coverage} object.
#'
#' @export
#'
coverage <- function(bricks_df, timeline, manifest = manifest()) {

    .check_bricks_df(bricks_df)
    .check_timeline(timeline, bricks_df)

    coverage <- structure(list(
        manifest = .as_lom(.as_df(manifest), .template.manifest),
        bands    = .as_lom(bricks_df, .template.bands),
        timeline = .as_lom(tibble::tibble(
            timeline = list(as.character(unlist(timeline, use.names = FALSE)))),
            .template.timeline),
        bricks   = .as_lom(bricks_df, .template.bricks)
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

    .check_coverage(coverage)

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
    print(.as_df(x[["bricks"]]))
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

    if (class(coverage) == "coverage") {

        return(TRUE)
    }

    .check_manifest(coverage[["manifest"]])
    .check_bands(coverage[["bands"]])
    .check_timeline(coverage[["timeline"]], bricks_df = .as_df(coverage[["bricks"]]))
    .check_bricks(coverage[["bricks"]])

    return(TRUE)
}

#' @title Internal coverage functions
#'
#' @name .check_manifest
#'
#' @description Check for validity of a coverages manifest list.
#'
#' @param manifest   A manifest \code{list} object.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_manifest <- function(manifest) {

    if (.is_empty(manifest)) {

        stop("The coverage manifest is empty.")
    }

    tryCatch(.as_lom(.as_df(manifest), .template.manifest),
             error = function(e) {

                 stop("Invalid coverage manifest data.")
             })

    return(TRUE)
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
#' @name .check_timeline
#'
#' @description Check for validity of a coverage timeline field.
#'
#' @param timeline    A \code{vector} sequence with dates.
#' @param bricks_df   A bricks \code{data.frame} object.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_timeline <- function(timeline, bricks_df) {

    if (.is_empty(timeline)) {

        stop("The timeline is empty.")
    }

    timeline <- list(as.character(unlist(timeline, use.names = FALSE)))

    if (any(bricks_df[["time_len"]] != length(timeline[[1]]))) {

        warning(sprintf(paste(
            "One or more bricks 'timeline' have inconsistent length (%s)",
            "with fetched 'time_len' metadata.",
            length(timeline[[1]])
        )), call. = FALSE)
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

    tryCatch(
        .as_lom(.as_df(bricks), .template.bricks),
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
#' @description Lits bricks from \code{coverage} object.
#'
#' @param coverage      A \code{coverage} object.
#' @param ...           A set of \code{names} or \code{character} vector with the bands to be returned.
#'
#' @return A \code{list} of bricks \code{data.frame} objects.
#'
#' @export
#'
get_bricks <- function(coverage, ...) {

    .check_coverage(coverage)

    bands_df <- .as_df(coverage[["bands"]])
    bricks <- coverage[["bricks"]]

    bricks <- lapply(seq_along(bricks), function(i) {
        b <- .attach_bands_info(.as_df(bricks[i]), bands_df)
        b <- .filter_bands(b, ...)
        return(b)
    })

    bricks <- tibble::as_tibble(do.call(rbind, bricks))
    bricks[["timeline"]] <- coverage[["timeline"]][["timeline"]]

    return(bricks)
}

#' @title Coverage functions
#'
#' @name get_bricks_list
#'
#' @description Lits rasters from \code{coverage} object.
#'
#' @param coverage      A \code{coverage} object.
#' @param ...           A set of \code{names} or \code{character} vector with the bands to be returned.
#'
#' @return A \code{list} of rasters \code{data.frame} objects.
#'
#' @export
#'
get_bricks_list <- function(coverage, ...) {

    .check_coverage(coverage)

    bands_df <- .as_df(coverage[["bands"]])
    bricks <- coverage[["bricks"]]

    bricks <- lapply(seq_along(bricks), function(i) {
        b <- .attach_bands_info(.as_df(bricks[i]), bands_df)
        b <- .filter_bands(b, ...)
        b[["timeline"]] <- coverage[["timeline"]][["timeline"]]
        return(b)
    })

    return(bricks)
}

#' @title Coverage functions
#'
#' @name fun_brick
#'
#' @description Returns an enclosure function that can be used as argument of
#' \code{apply_bricks} and \code{apply_bricks_cluster} functions' parameter \code{fun}.
#' Parameters passed in \code{...} can be viewed in \code{expr} code.
#'
#' @param expr   An R language expression to be evaluated as the function body.
#' @param ...    Any additional arguments to be included in function evaluation.
#'
#' @return The evaluation of \code{expr} expression.
#'
#' @export
#'
fun_brick <- function(expr, ...) {

    expr <- substitute(expr)
    fun_dots <- list(...)

    function(...) {

        eval(expr, append(as.list(substitute(list(...)))[-1:0], fun_dots))
    }
}

#' @title Coverage functions
#'
#' @name apply_bricks
#'
#' @description Apply a function on each \code{coverage}'s bricks object. Parameter
#' \code{fun} is a function with bricks attributes names as parameters. A helper
#' function \code{fun_brick} can be used to create a prototype of this function easily.
#'
#' @param coverage   A \code{coverage} object.
#' @param fun        A function to be called for each brick that receives the bricks' attributes.
#' @param ...        A set of \code{names} or \code{character} vector with the bands to be returned.
#'
#' @return A rasters \code{data.frame} object.
#'
#' @export
#'
apply_bricks <- function(coverage, fun, ...) {

    lapply(get_bricks_list(coverage, ...), function(b) {

        return(do.call(fun, as.list(b)))
    })
}

#' @title Coverage functions
#'
#' @name apply_bricks_cluster
#'
#' @description Apply a function on each \code{coverage}'s bricks object. Parameter
#' \code{fun} is a function with bricks attributes names as parameters. A helper
#' function \code{func_brick} can be used to create a prototype of this function easily.
#' The function creates processing clusters using package \code{parallel}.
#'
#' @param coverage       A \code{coverage} object.
#' @param fun            A function to be called for each brick that receives the bricks' attributes.
#' @param ...            A set of \code{names} or \code{character} vector with the bands to be returned.
#' @param clusters       An \code{integer} with the number of clusters to be created.
#' @param cluster_type   A \code{character} text with the type of cluster to be created.
#'
#' @return A rasters \code{data.frame} object.
#'
#' @export
#'
apply_bricks_cluster <- function(coverage, fun, ..., clusters = 1, cluster_type = c("PSOCK", "FORK")) {

    if (!requireNamespace("parallel", quietly = TRUE)) {

        message("Unable to load `parallel` package. Running serial `apply_bricks`...")
        return(apply_bricks(coverage, fun, ...))
    }

    cluster_type <- match.arg(cluster_type, c("PSOCK", "FORK"))

    res <- tryCatch({

        message(sprintf("Creating %s '%s' cluster(s)...", clusters, cluster_type))
        cl <- parallel::makeCluster(clusters, type = cluster_type)

        message("Exporting objects to cluster(s)...")
        parallel::clusterExport(cl, "fun", environment())

        message("Starting processes...")
        res <- parallel::clusterApply(
            cl, get_bricks_list(coverage, ...), function(b) {

            return(do.call(fun, as.list(b)))
        })
        return(res)
    },
    error = function(e) {

        stop(e$message)
    },
    finally = parallel::stopCluster(cl))
    message("Processes finished.")

    return(res)
}
