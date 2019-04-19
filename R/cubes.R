#' @title Cube functions
#'
#' @name cube_functions
#'
#' @param name   A \code{character} text with cube name.
#' @param remote   A \code{character} text with remote name.
#' @param cube   An \code{EOCubes_cube} object.
#'
#' @description These functions provides the basic operations over a cube
#' (\code{EOCubes_cube}) object.
#'
#' @seealso \code{\link{remote}}
#'
#' @examples
#' x <- remote("localhost")
#' cub1 <- cube("MOD13Q1/006", x)
#' cube_name(cub1)   # show the entry name 'MOD13Q1/006'
#' cube_bands(cub1)  # show bands and its meta data
#' cube_crs(cub1)    # show crs string
#'
NULL

#' @describeIn cube_functions Fetches a cube from remote.
#'
#' @return An \code{EOCubes_cube} object.
#'
#' @export
#'
cube <- function(name, remote = default_remote()) {

    if (!("EOCubes_remote" %in% class(remote)))
        stop("You must inform an `EOCubes_remote` object as data input.", call. = FALSE)

    if (missing(remote))
        message(sprintf("Searching cube in default remote '%s'.", remote_name(remote)))

    if (!(name %in% names(remote$cubes)))
        stop(sprintf("Cube '%s' not found in remote '%s'.", name, remote_name(remote)), call. = FALSE)

    res <- .open_json(remote$cubes[[name]]$href)

    res <- structure(res,
                     cube_name = name,
                     remote_name = remote_name(remote),
                     class = "EOCubes_cube")

    return(res)
}

#' @describeIn cube_functions Returns the name of a cube.
#'
#' @return A \code{character} string.
#'
#' @export
#'
cube_name <- function(cube) {

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    return(attr(cube, "cube_name"))
}

#' @describeIn cube_functions Lists the all registered bands in a cube.
#'
#' @return An \code{EOCubes_bandlist} object.
#'
#' @export
#'
cube_bands <- function(cube) {

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    res <- cube$meta$bands

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

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    return(cube$meta$crs$properties$name)
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
#' @param prefix   A \code{character} vector containing tile name prefix to be
#' filtered.
#' @param tiles   A \code{EOCubes_tilelist} object.
#'
#' @seealso \code{\link{remote}}, \code{\link{cube}}
#'
#' @examples
#' x <- remote("localhost")
#' cub1 <- cube("MOD13Q1/006", x)
#' y <- list_tiles(cub1, "h13v10") # select 'h13v10' tile
#'
#' if (require("tibble") && require("sf")) { # required by `sf`
#'
#'    tiles_to_sfc(y)  # requires `sf` package
#'    file <- system.file("shape/example.shp", package = "EOCubes")
#'    cub2 <- filter_tiles(cub1, geom = file)  # requires `sf` package
#'    list_tiles(cub2)
#' }
#'
NULL

#' @describeIn tiles_functions Returns a \code{EOCubes_cube} with tiles that
#' intersects the \code{geom} object.
#'
#' @return An \code{EOCubes_cube} object.
#'
#' @param ...   Any parameter to be passed to \code{sf::read_sf} function, in
#' case of \code{geom} is a path to a shapefile.
#'
#' @details
#' The function \code{filter_tiles} requires \code{sf} package.
#'
#' @export
#'
filter_tiles <- function(cube, prefix = NULL, geom = NULL, ...) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    tiles = list_tiles(cube = cube, prefix = prefix)

    if (is.null(tiles)) {

        cube$tiles <- NULL
        return(cube)
    }

    if (!is.null(geom)) {

        if (is.character(geom))
            geom <- sf::read_sf(geom)

        if (!any(c("sfc", "sf") %in% class(geom)))
            stop("`geom` parameter must be a `sfc` or `sf` object from `sf` package.", call. = FALSE)

        geom <- sf::st_transform(geom, crs = cube_crs(cube = cube))
        extents <- tiles_to_sfc(tiles = tiles)

        selected <- c(sf::st_intersects(extents, geom, sparse = FALSE) & !sf::st_touches(extents, geom, sparse = FALSE))
        if (length(selected) == 0) {

            warning(sprintf("No tile intersects the informed geometry."), call. = FALSE)
            cube$tiles <- NULL
            return(cube)
        }

        tiles <- tiles[selected]
    }

    class(tiles) <- "list"
    cube$tiles <- tiles
    return(cube)
}

#' @describeIn tiles_functions Fetches all registered tiles in a cube.
#'
#' @return An \code{EOCubes_fetched, EOCubes_cube} object.
#'
#' @details
#' Fetching tiles can be an expensive task. You can filter tiles that intersects
#' your studing area before the fetching procedure using \code{filter_tiles}
#' function.
#'
#' @export
#'
fetch_tiles <- function(cube) {

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    # fetch tiles
    cube$tiles <- lapply(cube$tiles, function(x) {

        if (is.null(x$href))
            return(x)
        .open_json(x$href)
    })

    class(cube) <- c("EOCubes_fetched", "EOCubes_cube")
    return(cube)
}

#' @describeIn tiles_functions Lists all registered tiles in a cube.
#'
#' @return An \code{EOCubes_tilelist} object or \code{NULL} if no tile
#' satisfies the filter criteria
#'
#' @export
#'
list_tiles <- function(cube, prefix = NULL) {

    if (!("EOCubes_cube" %in% class(cube)))
        stop("You must inform an `EOCubes_cube` object as data input.", call. = FALSE)

    if (is.null(cube$tiles)) {

        warning("The informed cube has no tile.", call. = FALSE)
        invisible(NULL)
    }

    if (is.null(prefix)) {

        res <- structure(cube$tiles, crs = cube_crs(cube), class = "EOCubes_tilelist")
        return(res)
    }

    res <- .filter_prefix(prefix, cube$tiles)

    if (length(res) == 0) {

        warning(sprintf("No tile with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(res, crs = cube_crs(cube), class = "EOCubes_tilelist")
    return(res)
}

#' @describeIn tiles_functions Converts tiles extents to a \code{sfc} object.
#'
#' @return A \code{sfc} object.
#'
#' @details
#' The function \code{tiles_to_sfc} requires \code{sf} package.
#'
#' @export
#'
tiles_to_sfc <- function(tiles) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (!("EOCubes_tilelist" %in% class(tiles)))
        stop("You must inform an `EOCubes_tilelist` object as data input.", call. = FALSE)

    res <- sf::st_sfc(lapply(tiles, function(x) {

        sf::st_polygon(list(matrix(unlist(x$extent$geometry$coordinates), ncol = 2, byrow = T)))
    }), crs = attr(tiles, "crs"))

    return(res)
}
