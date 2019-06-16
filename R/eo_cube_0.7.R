new_eo_cube_0.7 <- function(files, tiles, bands, dates, files_url, bands_info, project_dir) {

    invisible(NULL)
}

cube.eo_cube <- function(cb, bands = NULL, from = NULL, to = NULL, geom = NULL, tiles = NULL) {

    cube_bands(cb) <- bands

    cube_interval(cb) <- interval(from, to)

    cube_geom(cb) <- geom

    cube_tiles(cb) <- tiles

    return(cb)
}

#### cube entry ####

as_entry.eo_cube <- function(cb) {

    res <- list(href = reference(cb), description = description(cb))

    return(res)
}

describe_entry.eo_cube <- function(en) {

    return(en$description)
}

check_entry.eo_cube <- function(en) {

    if (is.null(en$href) || is.null(en$description))
        stop("Invalid entry.", call. = FALSE)

    invisible(NULL)
}

open_entry.eo_cube <- function(en) {

    con <- tryCatch(
        suppressWarnings(file(en$href)),
        error = function(e) {

            stop(sprintf(paste("Invalid file location '%s'.",
                               "Reported error: %s"), en$href, e$message), call. = FALSE)
        })

    res <- cube(con)

    close(con)

    return(res)
}

#### cube catalog ####

check.eo_cube <- function(cb) {

    if (is.null(cb$id) || is.null(cb$meta) || is.null(cb$tiles) || is.null(names(cb$tiles)) ||
        any(sapply(cb$meta, function(x) (is.null(x$crs) || is.null(x$bands) ||
                                         is.null(x$raster) || is.null(x$extent) ||
                                         is.null(x$interval)))) ||
        any(sapply(cb$tiles, function(x) (is.null(x$href) || is.null(x$extent)))))
        stop("Invalid cube file definition.", call. = FALSE)

    return(cb)
}

#### cube get/set ####
description.eo_cube <- function(cb) {

    return(cb$description)
}

cube_name.eo_cube <- function(cb) {

    return(cb$id)
}

cube_bands.eo_cube <- function(cb) {

    attr(cb, "bands", TRUE) <- ifnull(attr(cb, "bands", TRUE), names(cb$meta$bands))

    return(attr(cb, "bands", TRUE))
}

`cube_bands<-.eo_cube` <- function(cb, bands) {

    if (is.null(bands))
        bands <- names(cb$meta$bands)

    if (any(!bands %in% names(cb$meta$bands)))
        stop("Band does not exist.", call. = FALSE)

    attr(cb, "bands", TRUE) <- ifnull(bands, attr(cb, "bands", TRUE))

    invisible(NULL)
}

interval.eo_cube <- function(cb) {

    attr(cb, "interval", TRUE) <- ifnull(attr(cb, "interval", TRUE), interval(from = cb$meta$interval$from, to = cb$meta$interval$to))

    return(attr(cb, "interval", TRUE))
}

`interval<-.eo_cube` <- function(cb, value) {

    if (is.null(value))
        value <- interval(from = cb$meta$interval$from, to = cb$meta$interval$to)

    if (!inherits(value, "interval"))
        stop("Invalid interval.", call. = FALSE)

    attr(cb, "interval", TRUE) <- value

    invisible(NULL)
}

cube_geom.eo_cube <- function(cb, tiles = NULL) {

    if (is.null(tiles))
        tiles <- rep(TRUE, length(cb$tiles))

    if ((is.logical(tiles) && length(tiles) != length(cb$tiles)) ||
        (is.numeric(tiles) && max(tiles) > length(cb$tiles)) ||
        (is.character(tiles) && any(!tiles %in% names(cb$tiles))))
        stop("Invalid tiles parameter.", call. = FALSE)

    attr(cb, "geom", TRUE) <- ifnull(attr(cb, "geom", TRUE), as_geometry(cb$tiles[tiles]))

    return(attr(cb, "geom", TRUE))
}

`cube_geom<-.eo_cube` <- function(cb, value) {

    # intersection with tiles geometries
    invisible(NULL)
}

cube_tiles.eo_cube <- function(cb) {

    attr(cb, "tiles", TRUE) <- ifnull(attr(cb, "tiles", TRUE), names(cb$tiles))

    return(attr(cb, "tiles", TRUE))
}

`cube_tiles<-.eo_cube` <- function(cb, value) {

    if (is.null(value))
        value <- names(cb$tiles)

    if (any(!value %in% names(cb$tiles)))
        stop("Invalid tile.", call. = FALSE)

    attr(cb, "tiles", TRUE) <- ifnull(attr(cb, "tiles", TRUE), names(cb$tiles))

    return(attr(cb, "tiles", TRUE))
}

cube_crs.eo_cube <- function(cb) {

    return(cb$meta$crs)
}

bbox.eo_cube <- function(cb) {

    # return bbox of geom
}

info_bands.eo_cube <- function(cb) {

    return(cb$meta$bands[cube_bands(cb)])
}

view.eo_cube <- function(cb, st_date, st_period, time_len) {


}
