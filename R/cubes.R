#' @title Cube functions
#'
#' @name cube_functions
#'
#' @param x   A \code{list} data structure to be converted
#' to \code{EOCubes_cube} object.
#' @param name   A \code{character} text with cube name.
#' @param repos   An \code{EOCubes_repository} object.
#' @param cube   An \code{EOCubes_cube} object.
#' @param tiles   A \code{logical} or \code{integer} vector indicating which
#' tile to be selected. If \code{NULL} (default) all tiles are selected.
#' @param from   A \code{Date} value to filter layers' dates.
#' @param to   A \code{Date} value to filter layers' dates.
#'
#' @description These functions provides the basic operations over a cube
#' (\code{EOCubes_cube}) object.
#'
#' @seealso \code{\link{repository}}
#'
#' @examples
#' x <- repository("AWS.S3")
#' cub1 <- cube("MOD13Q1/006", x)
#' cube_name(cub1)   # show the entry name 'MOD13Q1/006'
#' cube_bands_info(cub1)   # show bands and its meta data
#' cube_crs(cub1)   # show CRS string
#' cube_bbox(cub1)   # show bbox values
#' cube_raster_info(cub1)   # show raster size and pixel resolution
#'
NULL

#' @describeIn cube_functions Fetches a cube from repository.
#'
#' @return An \code{EOCubes_cube} object.
#'
#' @export
#'
cube <- function(name, repos = repos("localhost")) {

    if (!inherits(repos, "EOCubes_repository"))
        stop("You must inform an `EOCubes_repository` object as data input.", call. = FALSE)

    if (!(name %in% names(repos$cubes)))
        stop(sprintf("Cube '%s' not found in repository '%s'.", name, repository_name(repos)), call. = FALSE)

    res <- .open_json(repos$cubes[[name]]$href, cache = is_caching(repos))

    res <- as_cube(res, name = repository_name(repos), caching = is_caching(repos))

    if (requireNamespace("sf", quietly = TRUE))
        attr(res, "sfc") <- .tiles_to_sfc(cube = res)

    return(res)
}

#' @describeIn cube_functions Returns the name of a cube.
#'
#' @return A \code{character} string.
#'
#' @export
#'
cube_name <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$id
    return(res)
}

#' @describeIn cube_functions Shows all names of registered bands in a cube.
#'
#' @return A \code{character} vector.
#'
#' @export
#'
cube_bands <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$meta$bands
    if (is.null(res))
        stop("Cube definition doesn't have a valid 'meta/bands' field.")

    return(names(res))
}

#' @describeIn cube_functions Lists attributes of all registered bands in a
#' cube.
#'
#' @return An \code{EOCubes_bandlist} object.
#'
#' @export
#'
cube_bands_info <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$meta$bands
    if (is.null(res))
        stop("Cube definition doesn't have a valid 'meta/bands' field.")

    res <- lapply(res, function(band) {

        band$min <- suppressWarnings(as.numeric(band$min))
        band$max <- suppressWarnings(as.numeric(band$max))
        band$fill <- suppressWarnings(as.numeric(band$fill))
        band$scale <- suppressWarnings(as.numeric(band$scale))
        return(band)
    })

    res <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), res))

    res <- structure(res, class = "EOCubes_bandlist")
    return(res)
}

#' @describeIn cube_functions Get the registered coordinate reference system (CRS) of a cube.
#'
#' @return A \code{character} string.
#'
#' @export
#'
cube_crs <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$meta$crs$properties$name
    if (is.null(res))
        stop("Cube definition doesn't have a valid 'meta/crs' field.")

    return(cube$meta$crs$properties$name)
}

#' @describeIn cube_functions Get the extent of a cube.
#'
#' @return A \code{list} object.
#'
#' @details The function \code{cube_extent} access the metadata field 'extent'
#' of an \code{EOCubes_cube} object that contains \code{xmin}, \code{ymin},
#' \code{xmax}, and \code{ymax} values.
#'
#' @export
#'
cube_bbox <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$meta$extent$bbox
    if (is.null(res) || length(res) != 4)
        stop("Cube definition doesn't have a valid 'meta/extent/bbox' field.")

    return(res)
}

#' @describeIn cube_functions Get the raster size and resolution of a cube.
#'
#' @return A \code{list} object.
#'
#' @details The function \code{cube_raster_info} access the metadata field
#' 'raster' of an \code{EOCubes_cube} object that contains \code{size} (pixels)
#' and \code{resolution} (CRS units per pixel) values.
#'
#' @export
#'
cube_raster_info <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$meta$raster
    if (is.null(res))
        stop("Cube definition doesn't have a valid 'meta/raster' field.")

    return(res)
}

#' @describeIn cube_functions Get the date interval (min, max) of all
#' registered tiles in a cube.
#'
#' @return A \code{list} object.
#'
#' @details The function \code{cube_dates_info} access the metadata field
#' 'interval' of an \code{EOCubes_cube} object that contains \code{from}
#' and \code{to} dates interval that encompass cube layers values.
#'
#' @export
#'
cube_dates_info <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$meta$interval
    if (is.null(res))
        stop("Cube definition doesn't have a valid 'meta/interval' field.", call. = FALSE)

    if (!is.null(res$from) && any(is.na(res$from <- as.Date(res$from, "%Y-%m-%d"))))
        stop("Cube definition doesn't have a valid 'meta/interval' field.", call. = FALSE)

    if (!is.null(res$to) && any(is.na(res$to <- as.Date(res$to, "%Y-%m-%d"))))
        stop("Cube definition doesn't have a valid 'meta/interval' field.", call. = FALSE)

    return(res)
}

#' @describeIn cube_functions Filter tiles in a cube.
#'
#' @return An \code{EOCubes_cube} object with tiles that satisfies the
#' \code{which} criteria.
#'
#' @details The parameter \code{tiles} of the function \code{cube_tiles} can
#' be used with \code{tiles_which} function.
#'
#' @export
#'
cube_filter <- function(cube, tiles = NULL, from = NULL, to = NULL) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (length(cube$tiles) == 0)
        stop("Informed cube has no tile.", call. = FALSE)

    which <- .verify_which(which = tiles, cube = cube)

    res <- cube

    if (!is.null(from) && any(is.na(from <- as.Date(from[[1]], "%Y-%m-%d"))))
        stop("Invalid date value for `from` parameter.", call. = FALSE)

    if (!is.null(to) && any(is.na(to <- as.Date(to[[1]], "%Y-%m-%d"))))
        stop("Invalid date value for `to` parameter.", call. = FALSE)

    if (!is.null(from))
        res$meta$interval$from <- from

    if (!is.null(to))
        res$meta$interval$to <- to

    if (is.null(which))
        return(res)

    res$tiles <- res$tiles[which]
    return(res)
}

#' @title Tiles functions
#'
#' @name tiles_functions
#'
#' @description These functions provide operations to work with tiles
#' registered in cubes.
#'
#' @param cube   A \code{EOCubes_cube} object.
#' @param geom   A \code{sfc}, a \code{sf} object, or a \code{character}
#' path to a shapefile to filter intersecting tiles.
#' @param which   A \code{logical} or \code{integer} vector indicating which
#' tile to be fetched. If \code{NULL} (default) all tiles are fetched.
#' @param prefix   A \code{character} vector containing tile's name prefixes
#' to be filtered.
#'
#' @seealso \code{\link{repository}}, \code{\link{cube}}, \code{\link{cube_filter}}
#'
#' @examples
#' x <- repository("localhost")
#' cub1 <- cube("MOD13Q1/006", x)
#' tiles_index <- tiles_which(cub1, "h13v10")   # select 'h13v10' tile
#'
#' # required by sf package
#' if (require("tibble") && require("sf")) {
#'
#'    tiles_sfc(cub1, which = tiles_index)
#'    file <- system.file("shape/example.shp", package = "EOCubes")
#'    tiles_index <- tiles_which(cub1, geom = file)   # select all tiles that intersect shapefile
#'    tiles_sfc(cub1, which = tiles_index)
#' }
#'
NULL

#' @describeIn tiles_functions Verifies if which parameter is valid for a given
#' list of tiles.
#'
#' @return Either a \code{logical} or \code{integer} vector.
#'
#' @details
#' The function \code{.verify_which} is a internal function used to verify the
#' consistency of \code{which} parameter of some tiles functions.
#'
.verify_which <- function(which, cube) {

    if (is.null(which))
        return(NULL)

    if (!is.logical(which) && !is.integer(which))
        stop("You must inform a `logical` or `integer` vector in `which` parameter.", call. = FALSE)

    if (is.logical(which) && length(which) != length(cube$tiles))
        stop("The `logical` vector in `which` parameter must have the same length as tiles.", call. = FALSE)

    if ((is.logical(which) && !any(which)) || (is.integer(which) && (length(which) == 0) || max(which) > length(cube$tiles)))
        warning("No tile extent coerced to `sfc` due to `which` parameter.")

    return(which)
}

#' @describeIn tiles_functions Returns a \code{logical} vector indicating
#' selected tiles that satisfies filter.
#'
#' @return A \code{logical} vector.
#'
#' @details
#' The function \code{tiles_which} requires \code{sf} package to compute
#' intersection. It can be used in \code{which} parameter of functions
#' \code{\link{cube_filter}}, \code{\link{stacks}}, \code{\link{tiles_which}}
#'
#' @export
#'
tiles_which <- function(cube, prefix = NULL, geom = NULL) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (length(cube$tiles) == 0)
        stop("Informed cube has no tile.", call. = FALSE)

    selected <- .select_prefix(prefix, cube$tiles)

    if (is.null(geom))
        return(selected)

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run geometric intersection.", call. = FALSE)

    if (is.character(geom))
        geom <- sf::read_sf(geom)

    if (!any(c("sfc", "sf") %in% class(geom)))
        stop("`geom` parameter must be a `sfc` or `sf` object from `sf` package.", call. = FALSE)

    geom <- sf::st_transform(geom, crs = cube_crs(cube = cube))

    sfc <- tiles_sfc(cube = cube)

    selected <- selected & c(sf::st_intersects(sfc, geom, sparse = FALSE) & !sf::st_touches(sfc, geom, sparse = FALSE))

    return(selected)
}

#' @describeIn tiles_functions Lists all registered tiles in a cube.
#'
#' @return An \code{EOCubes_tilelist} object that lists all tiles from a cube.
#'
#' @export
#'
list_tiles <- function(cube) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (length(cube$tiles) == 0)
        stop("Informed cube has no tile.", call. = FALSE)

    res <- structure(cube$tiles, class = "EOCubes_tilelist")

    return(res)
}

#' @describeIn tiles_functions Fetches tiles in a cube.
#'
#' @param cache   A \code{logical} or \code{NULL} to enable (\code{TRUE}),
#' disable (\code{FALSE}), or proceed the default behavior of (\code{NULL})
#' the cache system.
#' @param progress_bar   A \code{logical} value indicating to show or not a
#' progress bar while fetching tiles.
#'
#' @return An \code{EOCubes_tiles} object.
#'
#' @details
#' Fetching tiles can be an expensive task. You can filter tiles that intersects
#' your area of interest by providing \code{which} parameter.
#'
.fetch_tiles <- function(cube, which = NULL, cache = NULL, progress_bar = FALSE) {

    if (length(cube$tiles) == 0)
        stop("Informed cube has no tile.", call. = FALSE)

    which <- .verify_which(which = which, cube = cube)

    tiles <- if (is.null(which)) cube$tiles else cube$tiles[which]

    if (is.null(cache))
        cache <- is_caching(cube)

    locations <- lapply(tiles, function(x) {

        x$href
    })

    res <- .open_multiple_jsons(locations = locations,
                                cache = cache,
                                progress_bar = progress_bar)

    res <- structure(res,
                     crs = cube_crs(cube),
                     class = "EOCubes_tiles")

    return(res)
}

#' @describeIn tiles_functions Returns all tiles extents in a \code{sfc} object.
#'
#' @return A \code{sfc} object from \code{sf} package.
#'
#' @details
#' The function \code{cubes_sfc} requires \code{sf} package.
#'
#' @export
#'
tiles_sfc <- function(cube, which = NULL) {

    if (!inherits(cube, "EOCubes_cube"))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- attr(cube, "sfc")
    if (is.null(res))
        stop("No `sfc` attribute found in this cube object.", call. = FALSE)

    which <- .verify_which(which = which, cube = cube)

    if (is.null(which))
        return(res)

    return(res[which])
}

#' @describeIn tiles_functions Converts tiles extents to a \code{sfc} object.
#'
#' @return A \code{sfc} object from \code{sf} package.
#'
#' @details
#' The function \code{tiles_to_sfc} requires \code{sf} package.
#'
.tiles_to_sfc <- function(cube, which = NULL) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    which <- .verify_which(which = which, cube = cube)

    tiles <- if (is.null(which)) cube$tiles else cube$tiles[which]

    res <- sf::st_sfc(lapply(tiles, function(tile) {

        sf::st_polygon(list(matrix(unlist(tile$extent$geometry$coordinates), ncol = 2, byrow = T)))
    }), crs = cube_crs(cube))

    return(res)
}

