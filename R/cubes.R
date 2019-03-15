#' @title Cubes management functions
#'
#' @name fetch
#'
#' @description Fetch the contents of a list of resources.
#'
#' @param data   A \code{list} of resources.
#'
#' @details Data is a \code{list} of resources returned by functions \code{list_*()}.
#'
#' @return A \code{list} of resource contents.
#'
.fetch <- function(data) {

    if (is.null(data))
        return(NULL)

    lapply(data, function(x) {

        .open_json(x$href)
    })
}

#' @title Cubes management functions
#'
#' @name list_layers
#'
#' @description Lists all registered layers and organizes it by date intervals.
#'
#' @param item   Any \code{cube*} data structure.
#' @param start_date   A \code{date} value to filter layers by date.
#' @param end_date   A \code{date} value to filter layers by date.
#'
#' @details The \code{start_date} and \code{end_date} parameters values can
#' be a Date or a character containing a date in format \code{"\%Y-\%m-\%d"}.
#'
#' @return A \code{list} of layers' urls pointing to raster resources
#' or \code{NULL} if \code{item} does not list layers.
#'
.list_layers <- function(item, start_date = NULL, end_date = NULL) {

    if (is.null(item$layers))
        return(NULL)

    timeline <- as.Date(.get_node(item$layers, "date"), "%Y-%m-%d")

    if (missing(start_date))
        start_date <- min(timeline)

    if (missing(end_date))
        end_date <- max(timeline)

    if (is.character(start_date))
        start_date <- as.Date(start_date, "%Y-%m-%d")

    if (is.character(end_date))
        end_date <- as.Date(end_date, "%Y-%m-%d")

    return(item$layers[timeline >= start_date & timeline <= end_date])
}

#' @title Cubes management functions
#'
#' @name fetch_layers
#'
#' @description Fetch selected layers from stacks.
#'
#' @param items   A \code{list} of any \code{cube*} data structure.
#' @param start_date   A \code{date} value to filter layers by date.
#' @param end_date   A \code{date} value to filter layers by date.
#' @param count   An \code{integer} with the mandatory number of layers for each stack.
#'
#' @return A \code{list} of layers references.
#'
.fetch_layers <- function(items, start_date = NULL, end_date = NULL, count = NULL) {

    if (any(is.null(.get_node(items, "layers"))))
        return(NULL)

    res <- lapply(items, function(item) {

        layers <- .list_layers(item = item, start_date = start_date, end_date = end_date)

        res <- list()
        if ((is.null(count) && length(layers) > 0) || (!is.null(count) && length(layers) == count))
            res <- list(href = .get_node(layers, "href"),
                        date = .get_node(layers, "date"))

        return(res)
    })

    return(Filter(function(x) { length(x) > 0 }, res))
}

#' @title Cubes management functions
#'
#' @name list_stacks
#'
#' @description Lists all registered stacks.
#'
#' @param item   Any \code{cube*} data structure.
#' @param prefix   A \code{character} vector containing stack name prefix(es)
#'
#' @return A \code{list} of stacks or \code{NULL} if \code{item} does not list stacks.
#'
.list_stacks <- function(item, prefix = NULL) {

    if (is.null(item$stacks))
        return(NULL)

    if (is.null(prefix))
        return(item$stacks)

    unlist(lapply(prefix, function(p) {

        select <- grepl(paste0("^", p, ".*$"), names(item$stacks))

        res <- item$stacks[select]

        if (length(res) == 0)
            warning(sprintf("No stack with prefix '%s' was found.", p), call. = FALSE)

        return(res)
    }), recursive = FALSE)
}

#' @title Cubes management functions
#'
#' @name fetch_stacks
#'
#' @description Fetch all registered stacks.
#'
#' @param items   A \code{list} of any \code{cube*} data structure.
#' @param prefix   A \code{character} vector containing stack name prefix(es)
#'
#' @return A \code{list} of stacks contents.
#'
.fetch_stacks <- function(items, prefix = NULL) {

    lapply(items, function(item) {

        .fetch(.list_stacks(item = item, prefix = prefix))
    })
}

#' @title Cubes management functions
#'
#' @name list_tiles
#'
#' @description Lists all registered tiles in a cube.
#'
#' @param item   Any \code{cube*} data structure.
#' @param prefix   A \code{character} vector containing tile name prefix(es)
#'
#' @return A \code{list} of tiles in a cube or \code{NULL} if \code{data} does not list tiles.
#'
.list_tiles <- function(item, prefix = NULL) {

    if (is.null(item$tiles))
        return(NULL)

    if (is.null(prefix))
        return(item$tiles)

    unlist(lapply(prefix, function(p) {

        select <- grepl(paste0("^", p, ".*$"), names(item$tiles))
        res <- item$tiles[select]

        if (length(res) == 0)
            warning(sprintf("No tile with prefix '%s' was found.", p), call. = FALSE)

        return(res)
    }), recursive = FALSE)
}

#' @title Cubes management functions
#'
#' @name fetch_tiles
#'
#' @description Fetch all registered tiles in a cube.
#'
#' @param items   A \code{list} of any \code{cube*} data structure.
#' @param prefix   A \code{character} vector containing tile name prefix(es)
#'
#' @return A \code{list} of tiles contents in a cube.
#'
.fetch_tiles <- function(items, prefix = NULL) {

    if (any(is.null(sapply(items, `[[`, "tiles"))))
        return(NULL)

    res <- lapply(items, function(item) {

        res <- .fetch(.list_tiles(item = item, prefix = prefix))
    })
}

#' @title Cubes management functions
#'
#' @name fetch_cube
#'
#' @description Fetches a registered cube
#'
#' @param name     A \code{character} text with cube name.
#' @param remote   A \code{character} text with remote name.
#'
#' @return A \code{cube} data structure.
#'
.fetch_cube <- function(name, remote = default_remote()) {

    if (missing(remote))
        message(sprintf("Listing cubes of default remote '%s'", remote))

    if (is.character(remote))
        remote <- get_remote(remote)

    if (!(name %in% names(remote$cubes)))
        stop(sprintf("Cube '%s' not found in remote '%s'.", name, remote$id))

    res <- .open_json(remote$cubes[[name]]$href)

    return(res)
}

#' @title Cubes management functions
#'
#' @name list_cubes
#'
#' @description Lists all registered cubes in a remote.
#'
#' @param remote   A \code{remote} data structure.
#' @param prefix   A \code{character} containing cube name prefix
#'
#' @return A \code{list} of cubes.
#'
#' @export
#'
list_cubes <- function(remote = default_remote(), prefix = NULL) {

    if (missing(remote))
        message(sprintf("Listing cubes of default remote '%s'", remote))

    if (is.character(remote))
        remote <- get_remote(remote)

    select <- grepl(paste0("^", prefix, ".*$"), names(remote$cubes))

    return(remote$cubes[select])
}

#' @title Cubes management functions
#'
#' @name with_tiles
#'
#' @description This is a helper function that construct a list of parameters
#' to be passed to \code{get_cube} function.
#'
#' @param prefix   A \code{character} vector containing tile name prefix(es)
#'
#' @return A \code{list} parameters to select tiles from cubes.
#'
#' @export
#'
with_tiles <- function(prefix = NULL) {

    res <- list()
    res$prefix = prefix

    return(res)
}

#' @title Cubes management functions
#'
#' @name with_stacks
#'
#' @description This is a helper function that construct a list of parameters
#' to be passed to \code{get_cube} function.
#'
#' @param prefix   A \code{character} vector containing stack name prefix(es)
#'
#' @return A \code{list} parameters to select stacks from tiles.
#'
#' @export
#'
with_stacks <- function(prefix = NULL) {

    res <- list()
    res$prefix = prefix

    return(res)
}

#' @title Cubes management functions
#'
#' @name with_layers
#'
#' @description This is a helper function that construct a list of parameters
#' to be passed to \code{get_cube} function.
#'
#' @param start_dates   A \code{date} vector to filter layers by date.
#' @param end_dates   A \code{date} vector to filter layers by date.
#' @param count   An \code{integer} with the mandatory number of layers in each interval.
#'
#' @return A \code{list} parameters to select layers from stacks.
#'
#' @details The \code{start_dates} and \code{end_dates} parameters values can
#' be a character vector containing dates in format \code{"\%Y-\%m-\%d"}.
#' Both parameters must have the same length.
#'
#' @export
#'
with_layers <- function(start_dates = NULL, end_dates = NULL, count = NULL) {

    res <- list()
    res$start_dates <- start_dates
    res$end_dates <- end_dates
    res$count <- count

    return(res)
}

#' @title Cubes management functions
#'
#' @name filter_cube
#'
#' @description Fetches a registered cube and its data.
#'
#' @param name   A \code{character} text with cube name.
#' @param remote   A \code{character} text with remote name.
#' @param tiles   A \code{list} data structure with parameters to select tiles from cube.
#' Used with \code{with_tiles()} function.
#' @param stacks   A \code{list} data structure with parameters to select stacks from tiles.
#' Used with \code{with_stacks()} function.
#' @param layers   A \code{list} data structure with parameters to select layers from stacks.
#' Used with \code{with_layers()} function.
#'
#' @return A \code{cube} data structure.
#'
#' @details The \code{start_dates} and \code{end_dates} parameters values can
#' be a character vector containing dates in format \code{"\%Y-\%m-\%d"}.
#'
#' @export
#'
filter_cube <- function(name,
                        remote = default_remote(),
                        tiles = with_tiles(),
                        stacks = with_stacks(c("ndvi", "evi")),
                        layers = with_layers(
                            start_dates = seq(as.Date("2000-09-01"), as.Date("2018-09-01"), by = "years"),
                            end_dates = seq(as.Date("2001-08-31"), as.Date("2019-08-31"), by = "years"),
                            count = 23)) {

    if (missing(remote))
        message(sprintf("Searching cube in default remote '%s'", remote))

    cube <- .fetch_cube(name = name, remote = remote)

    tiles <- .fetch_tiles(items = list(cube), prefix = tiles$prefix)

    tiles_stacks <- lapply(tiles, .fetch_stacks, prefix = stacks$prefix)

    res <- mapply(function(start_date, end_date) {

        # for each cube ...
        lapply(tiles_stacks, function(tiles) {

            # for each tile ...
            lapply(tiles, function(stacks) {

                layers <- .fetch_layers(items = stacks, start_date = start_date, end_date = end_date,
                                        count = layers$count)
            })
        })
    }, layers$start_dates, layers$end_dates, SIMPLIFY = FALSE, USE.NAMES = FALSE)

    return(res)
}



#'
#' #' @title Internal coverage functions
#' #'
#' #' @name .check_bricks
#' #'
#' #' @description Check for validity of a coverages bricks list.
#' #'
#' #' @param bricks        A bricks \code{list} object.
#' #'
#' #' @return \code{TRUE} if pass in all checks.
#' #'
#' .check_bricks <- function(bricks) {
#'
#'     if (.is_empty(bricks)) {
#'
#'         stop("The coverage bricks list is empty.")
#'     }
#'
#'     tryCatch(
#'         .as_lom(.as_df(bricks), .template.bricks),
#'         error = function(e) {
#'
#'             stop(sprintf("Invalid coverage bricks data with error: \"%s\".",
#'                          e$message))
#'         })
#'
#'     return(TRUE)
#' }
#'
#' #' @title Coverage functions
#' #'
#' #' @name get_bricks
#' #'
#' #' @description Lits bricks from \code{coverage} object.
#' #'
#' #' @param coverage      A \code{coverage} object.
#' #' @param ...           A set of \code{names} or \code{character} vector with the bands to be returned.
#' #'
#' #' @return A \code{list} of bricks \code{data.frame} objects.
#' #'
#' #' @export
#' #'
#' get_bricks <- function(coverage, ...) {
#'
#'     .check_coverage(coverage)
#'
#'     bands_df <- .as_df(coverage[["bands"]])
#'     bricks <- coverage[["bricks"]]
#'
#'     bricks <- lapply(seq_along(bricks), function(i) {
#'         b <- .attach_bands_info(.as_df(bricks[i]), bands_df)
#'         b <- .filter_bands(b, ...)
#'         return(b)
#'     })
#'
#'     bricks <- tibble::as_tibble(do.call(rbind, bricks))
#'     bricks[["timeline"]] <- coverage[["timeline"]][["timeline"]]
#'
#'     return(bricks)
#' }
#'
#' #' @title Coverage functions
#' #'
#' #' @name get_bricks_list
#' #'
#' #' @description Lits rasters from \code{coverage} object.
#' #'
#' #' @param coverage      A \code{coverage} object.
#' #' @param ...           A set of \code{names} or \code{character} vector with the bands to be returned.
#' #'
#' #' @return A \code{list} of rasters \code{data.frame} objects.
#' #'
#' #' @export
#' #'
#' get_bricks_list <- function(coverage, ...) {
#'
#'     .check_coverage(coverage)
#'
#'     bands_df <- .as_df(coverage[["bands"]])
#'     bricks <- coverage[["bricks"]]
#'
#'     bricks <- lapply(seq_along(bricks), function(i) {
#'         b <- .attach_bands_info(.as_df(bricks[i]), bands_df)
#'         b <- .filter_bands(b, ...)
#'         b[["timeline"]] <- coverage[["timeline"]][["timeline"]]
#'         return(b)
#'     })
#'
#'     return(bricks)
#' }
#'
#' #' @title Coverage functions
#' #'
#' #' @name fun_brick
#' #'
#' #' @description Returns an enclosure function that can be used as argument of
#' #' \code{apply_bricks} and \code{apply_bricks_cluster} functions' parameter \code{fun}.
#' #' Parameters passed in \code{...} can be viewed in \code{expr} code.
#' #'
#' #' @param expr   An R language expression to be evaluated as the function body.
#' #' @param ...    Any additional arguments to be included in function evaluation.
#' #'
#' #' @return The evaluation of \code{expr} expression.
#' #'
#' #' @export
#' #'
#' fun_brick <- function(expr, ...) {
#'
#'     expr <- substitute(expr)
#'     fun_dots <- list(...)
#'
#'     function(...) {
#'
#'         eval(expr, append(as.list(substitute(list(...)))[-1:0], fun_dots))
#'     }
#' }
#'
#' #' @title Coverage functions
#' #'
#' #' @name apply_bricks
#' #'
#' #' @description Apply a function on each \code{coverage}'s bricks object. Parameter
#' #' \code{fun} is a function with bricks attributes names as parameters. A helper
#' #' function \code{fun_brick} can be used to create a prototype of this function easily.
#' #'
#' #' @param coverage   A \code{coverage} object.
#' #' @param fun        A function to be called for each brick that receives the bricks' attributes.
#' #' @param ...        A set of \code{names} or \code{character} vector with the bands to be returned.
#' #'
#' #' @return A rasters \code{data.frame} object.
#' #'
#' #' @export
#' #'
#' apply_bricks <- function(coverage, fun, ...) {
#'
#'     lapply(get_bricks_list(coverage, ...), function(b) {
#'
#'         return(do.call(fun, as.list(b)))
#'     })
#' }
#'
#' #' @title Coverage functions
#' #'
#' #' @name apply_bricks_cluster
#' #'
#' #' @description Apply a function on each \code{coverage}'s bricks object. Parameter
#' #' \code{fun} is a function with bricks attributes names as parameters. A helper
#' #' function \code{func_brick} can be used to create a prototype of this function easily.
#' #' The function creates processing clusters using package \code{parallel}.
#' #'
#' #' @param coverage       A \code{coverage} object.
#' #' @param fun            A function to be called for each brick that receives the bricks' attributes.
#' #' @param ...            A set of \code{names} or \code{character} vector with the bands to be returned.
#' #' @param clusters       An \code{integer} with the number of clusters to be created.
#' #' @param cluster_type   A \code{character} text with the type of cluster to be created.
#' #'
#' #' @return A rasters \code{data.frame} object.
#' #'
#' #' @export
#' #'
#' apply_bricks_cluster <- function(coverage, fun, ..., clusters = 1, cluster_type = c("PSOCK", "FORK")) {
#'
#'     if (!requireNamespace("parallel", quietly = TRUE)) {
#'
#'         message("Unable to load `parallel` package. Running serial `apply_bricks`...")
#'         return(apply_bricks(coverage, fun, ...))
#'     }
#'
#'     cluster_type <- match.arg(cluster_type, c("PSOCK", "FORK"))
#'
#'     res <- tryCatch({
#'
#'         message(sprintf("Creating %s '%s' cluster(s)...", clusters, cluster_type))
#'         cl <- parallel::makeCluster(clusters, type = cluster_type)
#'
#'         message("Exporting objects to cluster(s)...")
#'         parallel::clusterExport(cl, "fun", environment())
#'
#'         message("Starting processes...")
#'         res <- parallel::clusterApply(
#'             cl, get_bricks_list(coverage, ...), function(b) {
#'
#'             return(do.call(fun, as.list(b)))
#'         })
#'         return(res)
#'     },
#'     error = function(e) {
#'
#'         stop(e$message)
#'     },
#'     finally = parallel::stopCluster(cl))
#'     message("Processes finished.")
#'
#'     return(res)
#' }
