new_eo_cube_0.7 <- function(files, tiles, bands, dates, files_url, bands_info, project_dir) {

    invisible(NULL)
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

    con <- new_connection(en$href)

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

description.eo_cube <- function(cb) {

    return(cb$description)
}

#### cube get/set ####

cube_name.eo_cube <- function(cb) {

    return(cb$id)
}

cube_bands.eo_cube <- function(cb) {

    if (is.null(attr(cb, "bands", TRUE)))
        attr(cb, "bands", TRUE) <- names(cb$meta$bands)

    return(attr(cb, "bands", TRUE))
}

`cube_bands<-.eo_cube` <- function(cb, bands) {

    if (is.null(bands))
        bands <- names(cb$meta$bands)

    if (any(is.na(bands)) || any(!bands %in% names(cb$meta$bands)))
        stop("Invalid `bands` list.", call. = FALSE)

    attr(cb, "bands", TRUE) <- ifnull(bands, names(cb$meta$bands))

    invisible(NULL)
}

cube_interval.eo_cube <- function(cb) {

    if (is.null(attr(cb, "interval", TRUE)))
        attr(cb, "interval", TRUE) <- interval(from = cb$meta$interval$from,
                                               to = cb$meta$interval$to)

    return(attr(cb, "interval", TRUE))
}

`cube_interval<-.eo_cube` <- function(cb, value) {

    if (is.null(value))
        value <- interval(from = cb$meta$interval$from, to = cb$meta$interval$to)

    if (!inherits(value, "interval"))
        stop("Invalid interval.", call. = FALSE)

    attr(cb, "interval", TRUE) <- value

    invisible(NULL)
}

cube_geom.eo_cube <- function(cb) {

    return(attr(cb, "geom", TRUE))
}

`cube_geom<-.eo_cube` <- function(cb, value) {

    if (is.null(value)) {

        value <- cube_bbox(cb)
    } else if (inherits(value, "bbox")) {

        # do nothing
    } else if (inherits(value, "character")) {

        if (!requireNamespace("sf", quietly = TRUE))
            stop("You need `sf` package to inform a geometry.", call. = FALSE)
        value <- sf::read_sf(value)

    } else if (inherits(value, c("sfc", "sf"))) {

        if (!requireNamespace("sf", quietly = TRUE))
            stop("You need `sf` package to inform a geometry.", call. = FALSE)
        # do nothing
    } else
        stop("Invalid `geom` value.", call. = FALSE)

    attr(cb, "geom", TRUE) <- value

    invisible(NULL)
}

cube_tiles.eo_cube <- function(cb) {

    if (is.null(attr(cb, "tiles", TRUE)))
        attr(cb, "tiles", TRUE) <- names(cb$tiles)

    return(attr(cb, "tiles", TRUE))
}

`cube_tiles<-.eo_cube` <- function(cb, value) {

    if (is.null(value))
        value <- names(cb$tiles)

    if (any(is.na(value)) || any(!value %in% names(cb$tiles)))
        stop("Invalid `tiles` list.", call. = FALSE)

    attr(cb, "tiles", TRUE) <- ifnull(attr(cb, "tiles", TRUE), names(cb$tiles))

    invisible(NULL)
}

cube_slices.eo_cube <- function(cb) {

    return(attr(cb, "slices", TRUE))
}

`cube_slices<-.eo_cube` <- function(cb, value) {

    if (!is.null(value) && !inherits(value, "slices"))
        stop("Invalid `slices` value.", call. = FALSE)

    attr(cb, "slices", TRUE) <- value

    invisible(NULL)
}

cube_crs.eo_cube <- function(cb) {

    return(cb$meta$crs)
}

cube_bbox.eo_cube <- function(cb) {

    return(bbox(cb$meta$extent$bbox))
}

info_bands.eo_cube <- function(cb) {

    return(cb$meta$bands[cube_bands(cb)])
}

fetch.eo_cube <- function(cb) {

    if (is.null(cube_slices(cb)))
        stop("Invalid `slices` value.", call. = FALSE)


}

#### tiles get/set ####

tiles_bbox.eo_cube <- function(cb, tiles = NULL) {

    if (is.null(tiles)) {

        tiles <- cb$tiles
    } else if ((is.logical(tiles) && length(tiles) != length(cb$tiles)) ||
               (is.numeric(tiles) && max(tiles) > length(cb$tiles)) ||
               (is.character(tiles) && any(!tiles %in% names(cb$tiles)))) {

        stop("Invalid `tiles` parameter.", call. = FALSE)
    } else
        tiles <- cb$tiles[tiles]

    res <- lapply(tiles, function(tile) {

        return(bbox(tile$extent$bbox))
    })

    return(res)
}

tiles_geom.eo_cube <- function(cb, tiles = NULL) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (is.null(tiles)) {

        tiles <- cb$tiles
    } else if ((is.logical(tiles) && length(tiles) != length(cb$tiles)) ||
               (is.numeric(tiles) && max(tiles) > length(cb$tiles)) ||
               (is.character(tiles) && any(!tiles %in% names(cb$tiles)))) {

        stop("Invalid `tiles` parameter.", call. = FALSE)
    } else
        tiles <- cb$tiles[tiles]

    res <- sf::st_sfc(lapply(tiles, function(tile) {

        sf::st_polygon(list(matrix(unlist(tile$extent$geometry$coordinates), ncol = 2, byrow = T)))
    }), crs = cube_crs(cb))

    return(res)
}
