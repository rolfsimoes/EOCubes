
cast.eo_cube_0.7 <- function(cb) {

    if (is.null(cb$id) || is.null(cb$meta) || is.null(cb$tiles) || is.null(names(cb$tiles)) ||
        any(sapply(cb$meta, function(x) (is.null(x$crs) || is.null(x$bands) ||
                                         is.null(x$raster) || is.null(x$extent) ||
                                         is.null(x$interval)))) ||
        any(sapply(cb$tiles, function(x) (is.null(x$href) || is.null(x$extent)))))
        stop("Invalid cube file definition.", call. = FALSE)

    return(cb)
}

entry.eo_cube_0.7 <- function(cb) {

    res <- list(type = class(cb), href = reference(cb), description = description(cb),
                class = class(cb))

    return(res)
}

open_entry.eo_cube_0.7 <- function(en) {

    con <- tryCatch(
        suppressWarnings(file(en$href)),
        error = function(e) {

            stop(sprintf(paste("Invalid file location '%s'.",
                               "Reported error: %s"), en$href, e$message), call. = FALSE)
        })

    res <- cube(con, select_bands = select_bands,
                interval_from = interval_from, interval_to = interval_to,
                in_geometry = in_geometry, select_tiles = select_tiles)

    close(con)

    return(res)
}

cube.eo_cube_0.7 <- function(cb, select_bands = NULL,
                             interval_from = NULL, interval_to = NULL,
                             in_geometry = NULL, select_tiles = NULL) {

    bands(cb) <- select_bands

    interval(cb) <- interval(interval_from, interval_to)

    geom(cb) <- in_geometry

    tiles(cb) <- select_tiles

    return(cb)
}

bbox.eo_cube_0.7 <- function(cb) {

    return(bbox(cb$meta$extent$bbox))
}

interval.eo_cube_0.7 <- function(cb) {

    return(interval(from = cb$meta$interval$from, to = cb$meta$interval$to))
}

crs.eo_cube_0.7 <- function(cb) {

    return(cb$meta$crs)
}

bands.eo_cube_0.7 <- function(cb) {

    return(names(cb$meta$bands))
}

tiles.eo_cube_0.7 <- function(cb) {

    return(names(cb$tiles))
}

geom.eo_cube_0.7 <- function(cb, tiles = NULL) {


}

info_bands.eo_cube_0.7 <- function(cb, bands = NULL) {


}

view.eo_cube_0.7 <- function(cb, st_date, st_period, time_len) {


}
