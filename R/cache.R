#' @title Caching functions
#'
#' @name cache_functions
#'
#' @description These functions provides basic procedures to work with the
#' \code{EOCubes} cache system.
#'
#' @param cube   A \code{EOCubes_cube} object.
#' @param which   A \code{logical} or \code{integer} vector indicating which
#' tile to be fetched. If \code{NULL} (default) all tiles are fetched.
#' @param x   Either a \code{EOCubes_repository} or \code{EOCubes_cube} object.
#'
#' @seealso \code{\link{repository}}
#'
#' @examples
#' # disable cache system for subsequent fetches on 'x' repository
#' x <- repository("localhost", FALSE)
#' # by default cache system is enabled
#' x <- repository("localhost")
#' # cache all cube tiles contents
#' cache_cube(cube("MOD13Q1/006", x))
#' # persists cached data
#' save_cache()
NULL

#' @describeIn cache_functions Persists all cached data.
#'
#' @return None
#'
#' @details
#' The \code{save_cache} function saves all cached data into
#' \code{EOCubes:::.local_base} directory.
#'
#' @export
#'
save_cache <- function() {

    base <- path.expand(.local_base)
    cache_file <- path.expand(sprintf("%s/cache.RData", base))
    save(list = ls(all.names = TRUE, envir = .cache), file = cache_file, envir = .cache)
}

#' @describeIn cache_functions Lists all cached entries.
#'
#' @return A \code{character} vector.
#'
#' @details
#' The function \code{cached_entries} shows all entries (JSON locations)
#' cached.
#'
#' @export
#'
cached_entries <- function() {

    res <- ls(all.names = TRUE, envir = .cache)
    return(res)
}

#' @describeIn cache_functions Fetches all tiles in a cube and cache its
#' contents.
#'
#' @return None
#'
#' @details
#' The function \code{cache_cube} caches and persists all cube tiles contents.
#' Fetching tiles can be an expensive task. You can filter tiles that intersects
#' your area of interest by providing \code{which} parameter.
#'
#' @export
#'
cache_cube <- function(cube, which = NULL) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    .fetch_tiles(cube = cube, which = which, cache = TRUE, progress_bar = TRUE)
    save_cache()
}

#' @describeIn cache_functions Discards all cached tiles in a cube.
#'
#' @return None
#'
#' @details
#' The function \code{cache_flush} discards all cached tiles of
#' a cube. If no cube is informed (default), all entries are flushed.
#' To persist the flush, you must call explicitly \code{save_cache} function.
#'
#' @export
#'
flush_cache <- function(cube = NULL, which = NULL) {

    if (!inherits(cube, "EOCubes_cube") && !is.null(cube))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (is.null(cube))
        entries <- ls(.cache, all.names = TRUE)
    else
        entries <- lapply(cube$tiles, function(tile) tile$href)

    rm(list = entries, envir = .cache)
}

#' @describeIn cache_functions Check if a given object (either
#' \code{EOCubes_repository} or \code{EOCubes_cube}) has cache system enabled.
#'
#' @return A \code{logical} value.
#'
#' @export
#'
is_caching <- function(x) {

    if (!inherits(x, "EOCubes_repository") &&
        !inherits(x, "EOCubes_cube"))
        stop(paste("You must inform either an `EOCubes_repository` or `EOCubes_cube`",
                   "object as data input."), call. = FALSE)

    res <- attr(x, "caching")

    if (is.null(res))
        res <-  FALSE

    return(res)
}
