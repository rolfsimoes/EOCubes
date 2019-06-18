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
                                         is.null(x$extent) || is.null(x$interval)))) ||
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

    if (!is.null(bands) && (any(is.na(bands)) || any(!bands %in% names(cb$meta$bands))))
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

    if (!is.null(value) && !inherits(value, "interval"))
        stop("Invalid interval.", call. = FALSE)

    attr(cb, "interval", TRUE) <- ifnull(value, interval(from = cb$meta$interval$from, to = cb$meta$interval$to))

    invisible(NULL)
}

cube_geom.eo_cube <- function(cb) {

    return(attr(cb, "geom", TRUE))
}

`cube_geom<-.eo_cube` <- function(cb, value) {

    if (inherits(value, "bbox")) {

        # do nothing
    } else if (inherits(value, "character")) {

        if (!requireNamespace("sf", quietly = TRUE))
            stop("You need `sf` package to inform a geometry.", call. = FALSE)

        value <- sf::st_transform(sf::read_sf(value), crs = cube_crs(cube = cube))
    } else if (inherits(value, c("sfc", "sf"))) {

        if (!requireNamespace("sf", quietly = TRUE))
            stop("You need `sf` package to inform a geometry.", call. = FALSE)

        value <- sf::st_transform(value, crs = cube_crs(cube = cube))
    } else if (!is.null(value))
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

    if (!is.null(value) && (any(is.na(value)) || any(!value %in% names(cb$tiles))))
        stop("Invalid `tiles` list.", call. = FALSE)

    attr(cb, "tiles", TRUE) <- ifnull(value, names(cb$tiles))

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

    if (inherits(cube_geom(cb), "bbox")) {

        b <- tiles_bbox(cb, tiles = cube_tiles(cb))
        tiles <- sapply(b, function(x) bbox_intersects(x, cube_geom(cb)) && !bbox_touches(x, cube_geom(cb)))
    } else if (inherits(cube_geom(cb), c("sf", "sfc"))) {

        if (!requireNamespace("sf", quietly = TRUE))
            stop("You need `sf` package to run this function.", call. = FALSE)

        g <- tiles_geom(cb, tiles = cube_tiles(cb))
        tiles <- sf::st_intersects(g, cube_geom(cb), sparse = FALSE) & !sf::st_touches(g, cube_geom(cb), sparse = FALSE)
    } else
        tiles <- rep(TRUE, length(cube_tiles(cb)))

    open_entry(cb)[tiles]

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
}

#### tile entry ####

describe_entry.eo_tile <- function(en) {


}

check_entry.eo_tile <- function(en) {


}

open_entry.eo_tile <- function(en) {


}

#### tile catalog ####

check.eo_tile <- function(tl) {


}

description.eo_tile <- function(tl) {


}

#### tiles get/set ####

tiles_bbox.eo_cube <- function(cb) {

    res <- lapply(cb$tiles[cube_tiles(cb)], function(tile) {

        return(bbox(tile$extent$bbox))
    })

    return(res)
}

tiles_geom.eo_cube <- function(cb) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    res <- lapply(cb$tiles[cube_tiles(cb)], function(tile) {

        sf::st_polygon(list(matrix(unlist(tile$extent$geometry$coordinates), ncol = 2, byrow = T)))
    })

    res <- sf::st_sfc(res, crs = cube_crs(cb))

    return(res)
}

fetch_tiles.eo_cube <- function(cb) {

    if (Sys.info()["sysname"] != "Windows" && requireNamespace("parallel", quietly = TRUE)) {

        cb$tiles <- parallel::mclapply(cb$tiles[cube_tiles(tiles)], function(x) {

            con <- new_connection(x$href)
            res <- open_json(con)
            close(con)

            return(res)
        }, mc.cores = 8)
    } else {

        cb$tiles <- lapply(cb$tiles[cube_tiles(tiles)], function(x) {

            con <- new_connection(x$href)
            res <- open_json(con)
            close(con)

            return(res)
        })
    }

    return(cb)
}
