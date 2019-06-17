cube <- function(...) {

    UseMethod("cube")
}

cube.eo_cube <- function(cb, bands = NULL, from = NULL, to = NULL, geom = NULL,
                         tiles = NULL, slices = NULL) {

    if (!missing(bands))
        cube_bands(cb) <- bands

    if (!missing(from) && !missing(to))
        cube_interval(cb) <- interval(from, to)

    if (!missing(geom))
        cube_geom(cb) <- geom

    if (!missing(tiles))
        cube_tiles(cb) <- tiles

    if (!missing(slices))
        cube_slices(cb) <- slices

    return(cb)
}

cube.connection <- function(con, bands = NULL, from = NULL, to = NULL,
                            geom = NULL, tiles = NULL, slices = NULL) {

    res <- cast(open_json(con), default_type = "eo_cube")

    res <- cube(res, bands = bands, from = from, to = to, geom = geom,
                tiles = tiles, slice = slices)

    return(res)
}

#### functions ####
bbox <- function(x) {

    x <- unlist(x, use.names = FALSE)

    if (!is.numeric(x) || any(is.na(x)) || length(x) != 4)
        stop("Invalid `bbox` value.", call. = FALSE)

    res <- c(xmin = x[[1]], ymin = x[[2]], xmax = x[[3]], ymax = x[[4]])
    class(res) <- "bbox"

    return(res)
}

bbox_as_sfc <- function(b, crs) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (!inherits(b, "bbox"))
        stop("Invalid `bbox` value.", call. = FALSE)

    res <- sf::st_sfc(list(sf::st_polygon(
        list(matrix(b[c(1,2,1,4,3,4,3,2,1,2)], ncol = 2, byrow = T)))),
        crs = crs)

    return(res)
}

bbox_intersects <- function(b1, b2) {

    if (!inherits(b1, "bbox") || !inherits(b2, "bbox"))
        stop("Invalid `bbox` value.", call. = FALSE)

    res <-
        ((b2[[1]] <= b1[[1]] && b1[[1]] <= b2[[3]]) ||
             (b2[[1]] <= b1[[3]] && b1[[3]] <= b2[[3]]) ||
             (b1[[1]] <= b2[[1]] && b2[[1]] <= b1[[3]]) ||
             (b1[[1]] <= b2[[3]] && b2[[3]] <= b1[[3]])) &&
        ((b2[[2]] <= b1[[2]] && b1[[2]] <= b2[[4]]) ||
             (b2[[2]] <= b1[[4]] && b1[[4]] <= b2[[4]]) ||
             (b1[[2]] <= b2[[2]] && b2[[2]] <= b1[[4]]) ||
             (b1[[2]] <= b2[[4]] && b2[[4]] <= b1[[4]]))

    return(res)
}

bbox_touches <- function(b1, b2) {

    if (!inherits(b1, "bbox") || !inherits(b2, "bbox"))
        stop("Invalid `bbox` value.", call. = FALSE)

    res <- bbox_intersects(b1, b2) &&
        b1[[1]] == b2[[3]] || b1[[3]] == b2[[1]] ||
        b1[[2]] == b2[[4]] || b1[[4]] == b2[[2]]

    return(res)
}

interval <- function(from, to) {

    if (!is.null(from) && (length(from) != 1 || is.na(from <- as.Date(from))))
        stop("Invalid `from` date.", call. = FALSE)

    if (!is.null(to) && (length(to) != 1 || is.na(to <- as.Date(to))))
        stop("Invalid `to` date.", call. = FALSE)

    res <- list(from = from, to = to)
    class(res) <- "interval"

    return(res)
}

slices <- function(start, time_length, step_period = "12 months") {

    if (is.null(start) || length(start) != 1 || is.na(as.Date(start)))
        stop("Invalid `start` date.", call. = FALSE)

    if (!is.numeric(time_length) || length(time_length) != 1 ||
        is.na(time_length) || time_length <= 0)
        stop("Invalid `time_length` value.", call. = FALSE)

    tryCatch(seq(start, by = step_period, length.out = 2),
             error = function(e) stop("Invalid `step_period` string.", call. = FALSE))

    res <- list(start = start, time_length = time_length,
                step_period = step_period)
    class(res) <- "slices"

    return(res)
}

#### cube get/set ####

cube_name <- function(...) {

    UseMethod("cube_name")
}

cube_bands <- function(...) {

    UseMethod("cube_bands")
}

`cube_bands<-` <- function(...) {

    UseMethod("cube_bands<-")
}

cube_interval <- function(...) {

    UseMethod("cube_interval")
}

`cube_interval<-` <- function(...) {

    UseMethod("cube_interval<-")
}

cube_geom <- function(...) {

    UseMethod("cube_geom")
}

`cube_geom<-` <- function(...) {

    UseMethod("cube_geom<-")
}

cube_tiles <- function(...) {

    UseMethod("cube_tiles")
}

`cube_tiles<-` <- function(...) {

    UseMethod("cube_tiles<-")
}

cube_slices <- function(...) {

    UseMethod("cube_slices")
}

`cube_slices<-` <- function(...) {

    UseMethod("cube_slices<-")
}

cube_crs <- function(...) {

    UseMethod("cube_crs")
}

cube_bbox <- function(...) {

    UseMethod("cube_bbox")
}

info_bands <- function(...) {

    UseMethod("info_bands")
}

fetch <- function(...) {

    UseMethod("fetch")
}

#### tiles get/set ####

tiles_bbox <- function(...) {

    UseMethod("tiles_bbox")
}

tiles_geom <- function(...) {

    UseMethod("tiles_geom")
}
