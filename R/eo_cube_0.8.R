
cast.eo_cube_0.8 <- function(cb) {

    if ((cb$type != "#cube") || is.null(cb$id) || is.null(cb$crs) || is.null(cb$bbox) ||
        is.null(cb$description) || is.null(cb$items) || is.null(names(cb$items)) ||
        any(sapply(cb$items, function(x) (is.null(x$href) || is.null(x$type) || is.null(x$extent)))))
        stop("Invalid cube file definition.", call. = FALSE)

    return(cb)
}

entry.eo_cube_0.8 <- function(cb) {

    res <- list(type = class(cb), href = reference(cb), description = description(cb),
                class = class(cb))

    return(res)
}

open_entry.eo_cube_0.8 <- function(en) {

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

cube.eo_cube_0.8 <- function(cb, select_bands = NULL,
                             interval_from = NULL, interval_to = NULL,
                             in_geometry = NULL, select_tiles = NULL) {

    bands(cb) <- select_bands

    interval(cb) <- interval(interval_from, interval_to)

    geom(cb) <- in_geometry

    tiles(cb) <- select_tiles

    return(cb)
}


bbox.eo_cube_0.8 <- function(cb) {

    return(bbox(cb$bbox))
}

interval.eo_cube_0.8 <- function(cb) {

    return(interval(from = cb$interval$from, to = cb$interval$to))
}

crs.eo_cube_0.8 <- function(cb) {

    return(cb$crs)
}

bands.eo_cube_0.8 <- function(cb) {

    return(names(cb$bands))
}

tiles.eo_cube_0.8 <- function(cb) {


}

geom.eo_cube_0.8 <- function(cb, tiles = NULL) {


}

info_bands.eo_cube_0.8 <- function(cb, bands = NULL) {


}

view.eo_cube_0.8 <- function(cb, st_date, st_period, time_len) {


}
