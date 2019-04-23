#' @title Stacks functions
#'
#' @name stacks_functions
#'
#' @description These functions provides basic operations to work with stacks.
#' Stacks are a set of GeoTif reference locations organized by tile,
#' date interval, and bands.
#'
#' @param cube   A \code{EOCubes_cube} data structure.
#' @param bands   A \code{character} vector with band names to be retrieved.
#' @param start_dates   A \code{Date} vector to filter layers by date.
#' @param end_dates   A \code{Date} vector to filter layers by date.
#' @param which   A \code{logical} or \code{integer} vector indicating which
#' tile to be stacked. If \code{NULL} (default) all tiles are stacked.
#' @param stacks   A \code{EOCubes_stacks} object.
#' @param stack_length   An \code{integer} indicating the length of stacks to
#' be filtered.
#' @param longitude   A \code{numeric} value informing the longitute (X).
#' @param latitude   A \code{numeric} value informing the latitude (Y).
#' @param crs   An \code{integer} (EPSG code) or \code{character} (proj4)
#'
#' @seealso \code{\link{remote}}, \code{\link{cube}}, \code{\link{tiles_which}}
#'
#' @examples
#' x <- remote("localhost")
#' cub1 <- cube("MOD13Q1/006", x)
#' tiles_index <- tiles_which(cub1, "h12v10")
#' stk1 <- stack_tiles(cub1, bands = c("evi", "ndvi"),
#'                     start_dates = "2014-01-01", end_dates = "2015-01-01",
#'                     which = tiles_index)
#' stack_length(stk1)
#' stack_prune(stk1, 23)
#'
NULL

#' @describeIn stacks_functions Returns a new \code{EOCubes_stacks} object that
#' represents stacks for each band and each date intervals.
#'
#' @return An \code{EOCubes_stacks} object.
#'
#' @details \code{start_dates} and \code{end_dates} must have the same length.
#' These dates define intervals that are used to filter layers that constructs
#' stacks. The parameter \code{which} can be used to filter which tiles must
#' be stacked, hence which tiles must be fetched. Fetching tiles can be an
#' expensive task. You can filter tiles that intersects your area of interest
#' by providing \code{which} parameter.
#'
#' @export
#'
stack_tiles <- function(cube, bands = names(cube_bands(cube = cube)),
                        start_dates = NULL, end_dates = NULL, which = NULL) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (length(cube$tiles) == 0)
        stop("Informed cube has no tile.", call. = FALSE)

    if (is.null(start_dates))
        start_dates <- cube_dates_info(cube = cube)$from

    if (!inherits(start_dates, "Date"))
        start_dates <- as.Date(start_dates, "%Y-%m-%d")

    if (is.null(end_dates))
        end_dates <- cube_dates_info(cube = cube)$to

    if (!inherits(end_dates, "Date"))
        end_dates <- as.Date(end_dates, "%Y-%m-%d")

    if (length(start_dates) != length(end_dates))
        stop("`start_dates` and `end_dates` must have the same length.", call. = FALSE)

    if (!all(bands %in% names(cube_bands(cube = cube))))
        stop(sprintf("Informed band(s) %s not found in cube definition.",
                     .sublime_list(bands[!(bands %in% names(cube_bands(cube = cube)))])), call. = FALSE)

    intervals <- mapply(c, start_dates, end_dates, SIMPLIFY = FALSE)
    names(intervals) <- paste(start_dates, end_dates, sep = "_")
    names(bands) <- bands

    if (is.null(which))
        which <- rep(TRUE, length(cube$tiles))

    res <- lapply(.fetch_tiles(cube = cube, which = which), function(tile) {

        data <- lapply(bands, function(band) {

            if (is.null(tile$bands))
                stop("Error reading tile content: 'bands' field not found.", call. = FALSE)

            layers <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), tile$bands[[band]]$layers))
            layers$date <- as.Date(layers$date, "%Y-%m-%d")
            return(layers)
        })

        stack <- lapply(intervals, function(interval) {

            bands <- lapply(data, function(band) {

                return(band$href[(interval[1] <= band$date) & (band$date <= interval[2])])
            })

            timeline <- unique(lapply(data, function(band) {

                return(band$date[(interval[1] <= band$date) & (band$date <= interval[2])])
            }))

            if (length(timeline) > 1)
                stop(sprintf("Inconsistent timelines detected in tile '%s' and interval name '%s'.",
                             tile$id, interval), call. = FALSE)

            timeline <- timeline[[1]]

            res <- list(bands = bands, timeline = timeline)
            return(res)
        })

        return(stack)
    })

    res <- structure(res, class = "EOCubes_stacks")

    return(res)
}

#' @describeIn stacks_functions Returns a list of \code{integer} with the
#' lengths of each stack.
#'
#' @return A \code{list} of \code{integer}.
#'
#' @export
#'
stack_length <- function(stacks) {

    if (!inherits(stacks, "EOCubes_stacks"))
        stop("You must inform an `EOCubes_stacks` object as data input.", call. = FALSE)

    res <- lapply(stacks, function(tile) {
        lapply(tile, function(interval) {

            length(interval$timeline)
        })
    })

    return(res)
}

#' @describeIn stacks_functions Returns an \code{EOCubes_stacks} object where
#' each stack has the same length given by \code{stack_length}.
#'
#' @return An \code{EOCubes_stacks} object.
#'
#' @export
#'
stack_prune <- function(stacks, stack_length) {

    if (!inherits(stacks, "EOCubes_stacks"))
        stop("You must inform an `EOCubes_stacks` object as data input.", call. = FALSE)

    res <- lapply(stacks, function(tile) {

        Filter(function(interval) {
            length(interval$timeline) == stack_length
        }, tile)
    })

    res <- structure(res, class = "EOCubes_stacks")
    return(res)
}

#' @describeIn stacks_functions Returns an \code{EOCubes_timeseries} object that
#' represents a set of band timeseries from some date intervals.
#'
#' @return An \code{EOCubes_timeseries} object.
#'
#' @details \code{start_dates} and \code{end_dates} must have the same length.
#' These dates define intervals that are used to filter layers that constructs
#' the time series for each band and interval. \code{longitude} and
#' \code{latitude} parameters must be in \code{crs} projection. The default
#' value for \code{crs} is the same as the \code{cube} crs.
#'
#' @export
time_series <- function(cube, longitude, latitude,
                        bands = names(cube_bands(cube)),
                        start_dates = NULL, end_dates = NULL, crs = cube_crs(cube)) {

    if (!requireNamespace("sf"))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (!requireNamespace("raster"))
        stop("You need `raster` package to run this function.", call. = FALSE)

    point <- sf::st_sfc(sf::st_point(c(longitude, latitude)), crs = crs)

    which <- tiles_which(cube = cube, geom = point)

    if (!any(which))
        stop("The point is outside the cube extent.", call. = FALSE)

    stack <- stack_tiles(cube = cube, bands = bands, start_dates = start_dates,
                         end_dates = end_dates, which = which)

    point <- sf::as_Spatial(point)

    res <- lapply(stack, function(tile) {
        lapply(tile, function(interval) {
            bands <- lapply(interval$bands, function(band) {
                res <- suppressWarnings(pbmcapply::pbmclapply(band, function(url) {

                    if (grepl("^https?://.+", url))
                        url <- paste0("/vsicurl/", url)

                    tryCatch({
                        raster <- raster::raster(url, RAT = FALSE, quick = TRUE)
                        raster::extract(raster, point)
                    },
                    error = function(e) NA
                    )
                }, mc.cores = 16))
                unlist(res)
            })

            res <- list(timeline = interval$timeline,
                        bands = bands)
            return(res)
        })
    })

    res <- structure(res, class = "EOCubes_timeseries")
    return(res)
}
