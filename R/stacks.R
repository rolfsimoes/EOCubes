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
#' @param stacks   A \code{EOCubes_stacks} object.
#' @param stack_length   An \code{integer} indicating the length of stacks to
#' be filtered.
#'
#' @seealso \code{\link{remote}}, \code{\link{cube}}
#'
#' @examples
#' x <- remote("localhost")
#' cub1 <- cube("MOD13Q1/006", x)
#' cub2 <- filter_tiles(cub1, "h13v10")
#' cub3 <- fetch_tiles(cub2)
#' stk1 <- stacks(cub3, "2010-09-01", "2011-09-01", c("evi", "ndvi"))
#' stacks_length(stk1)
#' stk2 <- stacks_prune(stk1, 23)
#'
NULL

#' @describeIn stacks_functions Returns a new \code{EOCubes_stacks} object that
#' represents stacks for each band and each date intervals.
#'
#' @return An \code{EOCubes_stacks} object.
#'
#' @details \code{start_dates} and \code{end_dates} must have the same length.
#' These dates define intervals that are used to filter layers that constructs
#' stacks.
#'
#' @export
#'
stacks <- function(cube, start_dates, end_dates, bands = names(cube_bands(cube = cube))) {

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (!("Date" %in% class(start_dates)))
        start_dates <- as.Date(start_dates, "%Y-%m-%d")

    if (!("Date" %in% class(end_dates)))
        end_dates <- as.Date(end_dates, "%Y-%m-%d")

    if (length(start_dates) != length(end_dates))
        stop("`start_dates` and `end_dates` must have the same length.", call. = FALSE)

    interval_names <- paste(start_dates, end_dates, sep = "_")

    if (!all(bands %in% names(cube_bands(cube = cube))))
        stop(sprintf("Informed band(s) %s not found in cube definition.",
                     .sublime_list(bands[!(bands %in% names(cube_bands(cube = cube)))])), call. = FALSE)

    if (!("EOCubes_fetched" %in% class(cube))) {
        warning(paste("Fetching cube dynamically.",
                      "To avoid this message, use `fetch_cube()` function before create stack."), call. = FALSE)
        cube <- fetch_tiles(cube = cube)
    }

    res <- lapply(cube$tiles, function(tile) {

        slices <- lapply(tile$bands[bands], function(band) {

            layers <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), band$layers))

            timeline <- as.Date(layers$date, "%Y-%m-%d")
            res <- lapply(seq_along(start_dates), function(i) {

                selected <- timeline >= start_dates[i] & timeline < end_dates[i]

                res <- list(href = layers$href[selected], timeline = timeline[selected])
                return(res)
            })

            names(res) <- interval_names
            return(res)
        })

        # rearrange sliced data into stack format
        stack <- lapply(interval_names, function(interval) {
            res <- lapply(bands, function(band) {

                return(slices[[band]][[interval]])
            })
            names(res) <- bands
            return(res)
        })
        names(stack) <- interval_names
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
stacks_length <- function(stacks) {

    if (!("EOCubes_stacks" %in% class(stacks)))
        stop("You must inform an `EOCubes_stacks` object as data input.")

    res <- lapply(stacks, function(tile) {
        lapply(tile, function(interval) {
            lapply(interval, function(band) {

                length(band$timeline)
            })
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
stacks_prune <- function(stacks, stack_length) {

    if (!("EOCubes_stacks" %in% class(stacks)))
        stop("You must inform an `EOCubes_stacks` object as data input.")

    res <- lapply(stacks, function(tile) {
        Filter(function(interval) {
            all(sapply(interval, function(band) {

                length(band$timeline) == stack_length
            }))
        }, tile)
    })

    res <- structure(res, class = "EOCubes_stacks")
    return(res)
}
