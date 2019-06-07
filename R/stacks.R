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
#' @param which   A \code{logical} or \code{integer} vector indicating which
#' tile to be stacked. If \code{NULL} (default) all tiles are stacked.
#' @param stack_length   A \code{numeric} positive value informing the length
#' of each stack.
#' @param start_reference   A \code{Date} value used to compute the start dates
#' of each break given the \code{starts_interval} parameter and the
#' timeline of each tile.
#' @param starts_interval   A \code{character} value containing the interval
#' between two consecutives breaks' start dates.
#' @param from   A \code{Date} value to filter dates.
#' @param to   A \code{Date} value to filter dates.
#' @param timeline   A \code{Date} vector representing all images' acquisition
#' dates.
#' @param stacks   A \code{EOCubes_stacks} object.
#' @param longitude   A \code{numeric} value informing the longitute (X).
#' @param latitude   A \code{numeric} value informing the latitude (Y).
#' @param crs   An \code{integer} (EPSG code) or \code{character} (proj4)
#'
#' @seealso \code{\link{repository}}, \code{\link{cube}}, \code{\link{tiles_which}}
#'
#' @examples
#' x <- repository("localhost")
#' cub1 <- cube("MOD13Q1/006", x)
#' tiles_index <- tiles_which(cub1, "h12v10")
#' stk1 <- stacks(cub1, bands = c("evi", "ndvi"),
#'                which = tiles_index,
#'                start_reference = "2014-01-01", stack_length = 23,
#'                starts_interval = "year", from = "2013-06-01",
#'                to = "2015-06-01")
#' stack_length(stk1)
#' prune_stacks(stk1, 23)
#'
NULL

#' @describeIn stacks_functions Returns a new \code{EOCubes_stacks} object that
#' represents stacks for each band and each date intervals.
#'
#' @return An \code{EOCubes_stacks} object.
#'
#' @details
#' The parameter \code{which} can be used to filter which tiles must
#' be stacked, hence which tiles must be fetched. Fetching tiles can be an
#' expensive task. You can filter tiles that intersects your area of interest
#' by providing \code{which} parameter. The parameters \code{from} and \code{to}
#' filter the range of timeline considered in stacking procedure. For all other
#' parameters see the details of \code{timeline_intervals} function.
#'
#' @export
#'
stacks <- function(cube, bands = NULL, which = NULL,
                   start_reference = NULL, stack_length = NULL,
                   starts_interval = "12 months", from = NULL, to = NULL) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (length(cube$tiles) == 0)
        stop("Informed cube has no tile.", call. = FALSE)

    if (is.null(bands))
        bands <- cube_bands(cube = cube)

    if (!all(bands %in% cube_bands(cube = cube)))
        stop(sprintf("Informed band(s) %s not found in cube definition.",
                     .sublime_list(bands[!(bands %in% cube_bands(cube = cube))])), call. = FALSE)

    if (!is.null(from) && any(is.na(from <- as.Date(from[[1]], "%Y-%m-%d"))))
        stop("Invalid date value for `from` parameter.", call. = FALSE)

    if (!is.null(to) && any(is.na(to <- as.Date(to[[1]], "%Y-%m-%d"))))
        stop("Invalid date value for `to` parameter.", call. = FALSE)

    if (is.null(from))
        from <- cube_dates_info(cube = cube)$from

    if (is.null(to))
        to <- cube_dates_info(cube = cube)$to

    names(bands) <- bands

    if (is.null(which))
        which <- rep(TRUE, length(cube$tiles))

    res <- lapply(.fetch_tiles(cube = cube, which = which), function(tile) {

        if (is.null(tile$bands))
            stop("Error reading tile content: 'bands' field not found.", call. = FALSE)

        data <- lapply(bands, function(band) {

            layers <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), tile$bands[[band]]$layers))

            if (!is.null(to) && any(is.na(layers$date <- as.Date(layers$date, "%Y-%m-%d"))))
                stop(sprintf("Inconsistent dates detected in tile '%s'", tile$id))

            filter_layers <- (from <= layers$date) & (layers$date <= to)
            layers$href <- layers$href[filter_layers]
            layers$date <- layers$date[filter_layers]

            return(layers)
        })


        timeline <- unique(lapply(data, function(band) {

            return(band$date)
        }))

        if (length(timeline) > 1)
            stop(sprintf("Inconsistent timelines detected in tile '%s'.", tile$id), call. = FALSE)

        timeline <- timeline[[1]]

        intervals <- timeline_intervals(timeline = timeline, stack_length = stack_length,
                                        start_reference = start_reference, starts_interval = starts_interval)

        stack <- lapply(intervals, function(interval) {

            bands <- lapply(data, function(band) {

                return(band$href[(interval$from <= band$date) & (band$date <= interval$to)])
            })

            timeline <- timeline[(interval$from <= timeline) & (timeline <= interval$to)]

            res <- list(bands = bands, timeline = timeline)
            return(res)
        })

        return(stack)
    })

    res <- structure(res, class = "EOCubes_stacks")

    return(res)
}

#' @describeIn stacks_functions Returns an \code{EOCubes_intervals} indicating
#' where to break the timeline.
#'
#' @return An \code{EOCubes_intervals} object.
#'
#' @details
#' The function \code{timeline_intervals} generate an \code{EOCubes_intervals}
#' object that represents a list of from/to dates indicating ranges. Each
#' interval is computed considering a start date of reference
#' (\code{start_reference}) which is used to mark many start dates throughout
#' the timeline, according to an interval given by \code{starts_interval}
#' parameter. Thereafter, the end 'marks' are computed so that each interval
#' have the length given by \code{stack_length} parameter.
#'
#' If no \code{start_reference} is informed, you must omit \code{stack_length}
#' and \code{starts_interval} parameters. In this case, the interval will
#' comprehends all timeline.
#'
#' @export
#'
timeline_intervals <- function(timeline, start_reference = NULL, stack_length = NULL,
                               starts_interval = NULL) {

    if (!is.null(timeline) && any(is.na(timeline <- as.Date(timeline, "%Y-%m-%d"))))
        stop("Invalid dates values for `timeline` parameter.", call. = FALSE)

    if (!is.null(start_reference)) {

        if (any(is.na(start_reference <- as.Date(start_reference[[1]], "%Y-%m-%d"))))
            stop("Invalid date value for `start_referente` parameter.")

        if (is.null(stack_length) || !is.numeric(stack_length) || (stack_length <= 0))
            stop("Invalid number for `stack_length` parameter.", call. = FALSE)

        # check `starts_interval`...
        tryCatch(seq(start_reference, by = starts_interval, length.out = 2),
                 error = function(e) stop("Invalid string for `starts_interval` parameter.", call. = FALSE))

        if (start_reference < timeline[1]) {

            start_reference <- seq(start_reference, timeline[1], by = starts_interval)
            start_reference <- seq(start_reference[length(start_reference)], by = starts_interval, length.out = 2)[2]
        } else if (start_reference > timeline[1]) {

            period2 <- strsplit(starts_interval, " ")[[1]]
            if (length(period2) == 1)
                period2 <- paste(-1, period2)
            else
                period2 <- paste(-as.integer(period2[1]), period2[length(period2)])

            start_reference <- seq(start_reference, timeline[1], by = period2)
            start_reference <- start_reference[length(start_reference)]
        }

        start_dates <- seq(start_reference, timeline[length(timeline)], by = starts_interval)
    } else {

        if (!is.null(stack_length) || !is.null(starts_interval))
            stop(paste("Inconsistent parameter values for `start_reference`, `stack_length`,",
                       "and `starts_interval`."), call. = FALSE)

        stack_length <- length(timeline)
        start_dates <- timeline[1]
    }

    res <- lapply(start_dates, function(d) {

        res <- timeline[which.min(abs(timeline - d)) + c(0, stack_length - 1)]
        return(res)
    })

    res <- Filter(function(x) !any(is.na(x)), res)

    names(res) <- sapply(res, function(x) paste(x[1], x[2], sep = "_"))

    res <- lapply(res, function(x) {

        res <- list(from = x[1], to = x[2])
        return(res)
    })

    res <- structure(res, class = "EOCubes_intervals")
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
prune_stacks <- function(stacks, stack_length) {

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
#' @details
#' The \code{from} and \code{to} parameters defines an interval to filter
#' timeseries length. The \code{longitude} and \code{latitude} parameters must
#' be in \code{crs} projection. The default value for \code{crs} is the same as
#' the \code{cube} crs.
#'
#' @export
time_series <- function(cube, longitude, latitude, bands = cube_bands(cube),
                        from = NULL, to = NULL, crs = cube_crs(cube)) {

    if (!requireNamespace("sf"))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (!requireNamespace("raster"))
        stop("You need `raster` package to run this function.", call. = FALSE)

    point <- sf::st_sfc(sf::st_point(c(longitude, latitude)), crs = crs)

    which <- tiles_which(cube = cube, geom = point)

    if (!any(which))
        stop("The point is outside the cube extent.", call. = FALSE)

    stack <- stacks(cube = cube, bands = bands, which = which,
                         start_reference = NULL, stack_length = NULL,
                         starts_interval = NULL, from = from, to = to)

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
