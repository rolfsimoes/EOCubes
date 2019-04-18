#' @title Cubes management functions
#'
#' @name fetch_cube
#'
#' @description Fetches a registered cube
#'
#' @param cube_ref   A \code{character} text with cube name.
#'
#' @return A \code{EOCubes_cube} data structure.
#'
.fetch_cube <- function(cube_ref) {

    res <- .open_json(cube_ref$href)

    res <- structure(res, class = "EOCubes_cube")
    return(res)
}

#' @title Cubes management functions
#'
#' @name .fetch_tiles
#'
#' @description Fetch all registered tiles in a cube.
#'
#' @param tile_lst   A \code{EOCube_tilelist} data structure.
#'
#' @return A \code{list} of tiles contents in a cube.
#'
.fetch_tiles <- function(tile_lst) {

    if (is.null(tile_lst)) {

        warning(sprintf("No tile to be fetched."))
        invisible(NULL)
    }

    res <- lapply(tile_lst, function(x) {

        res <- .open_json(x$href)
    })

    names(res) <- names(tile_lst)

    return(res)
}

#' @title Cubes management functions
#'
#' @name list_cubes
#'
#' @description Lists all registered cubes in a remote.
#'
#' @param remote   A \code{remote} data structure.
#' @param prefix   A \code{character} containing cube name prefix to be filtered.
#'
#' @return A \code{list} of cubes.
#'
#' @export
#'
list_cubes <- function(remote = default_remote(), prefix = NULL) {

    if (missing(remote))
        message(sprintf("Listing cubes of default remote: '%s'", remote))

    if (is.character(remote))
        remote <- .fetch_remote(remote)

    res <- .filter_prefix(prefix, remote$cubes)

    if (length(res) == 0) {

        warning(sprintf("No cube with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(res, class = "EOCubes_cubelist")
    return(res)
}

#' @title Cubes management functions
#'
#' @name cube
#'
#' @description Fetches a registered cube and its data.
#'
#' @param name   A \code{character} text with cube name.
#' @param remote   A \code{character} text with remote name.
#'
#' @return A \code{EOCubes_cube} data structure.
#'
#' @export
#'
cube <- function(name, remote = default_remote()) {

    if (missing(remote))
        message(sprintf("Searching cube in default remote: '%s'", remote))

    if (is.character(remote))
        remote <- .fetch_remote(remote)

    if (!(name %in% names(remote$cubes)))
        stop(sprintf("Cube '%s' not found in remote: '%s'.", name, remote$id))

    res <- .fetch_cube(cube_ref = remote$cubes[[name]])

    return(res)
}

#' @title Cubes management functions
#'
#' @name cube_bands
#'
#' @description Lists the registered bands of a cube.
#'
#' @param cube   An \code{EOCubes_cube} data structure.
#'
#' @return A \code{list} with all registered bands in the given cube.
#'
#' @export
#'
cube_bands <- function(cube) {

    if (!"EOCubes_cube" %in% class(cube))
        stop("You must inform a `EOCubes_cube` as data input.")

    res <- cube$meta$bands

    res <- structure(res, class = "EOCubes_bandlist")
    return(res)
}

#' @title Cubes management functions
#'
#' @name cube_crs
#'
#' @description Get the registered coordinate reference system of a cube.
#'
#' @param cube   An \code{EOCubes_cube} data structure.
#'
#' @return A CRS \code{character} string of a given cube.
#'
#' @export
#'
cube_crs <- function(cube) {

    if (!"EOCubes_cube" %in% class(cube))
        stop("You must inform a `EOCubes_cube` as data input.")

    return(cube$meta$crs$properties$name)
}

#' @title Cubes management functions
#'
#' @name tiles_extents
#'
#' @description Lists all registered tiles in a cube.
#'
#' @param cube   A \code{EOCubes_cube} data structure.
#'
#' @return A \code{sfc} data structure with extents of all tiles in a cube.
#'
#' @export
#'
tiles_extents <- function(cube) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.")

    if (!"EOCubes_cube" %in% class(cube))
        stop("You must inform a `EOCubes_cube` as data input.")

    if (is.null(cube$tiles)) {

        stop("The informed cube has no tile.", call. = FALSE)
    }

    res <- sf::st_sfc(lapply(cube$tiles, function(x) {

        sf::st_polygon(list(matrix(unlist(x$extent$geometry$coordinates), ncol = 2, byrow = T)))
    }), crs = cube_crs(cube = cube))

    return(res)
}

#' @title Cubes management functions
#'
#' @name tiles_intersects
#'
#' @description Returns a \code{logical} vector indicating which tiles intersects \code{geom}.
#'
#' @param cube   A \code{EOCubes_cube} data structure.
#' @param geom   A \code{sfc} or \code{sf} feature data from \code{sf} package to filter tiles.
#'
#' @return A \code{logical} vector.
#'
#' @details
#' If \code{cube} parameter is missing, the function will try to
#' retrieve the \code{EOCubes_cube} from the parent frame.
#'
#' @export
#'
tiles_intersects <- function(cube, geom) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.")

    if (!"EOCubes_cube" %in% class(cube))
        stop("You must inform a `EOCubes_cube` as data input.")

    if (!any(c("sfc", "sf") %in% class(geom)))
        stop("`geom` parameter must be a `sfc` or `sf` object class from `sf` package.")

    geom <- sf::st_transform(geom, crs = cube_crs(cube = cube))
    extents <- tiles_extents(cube = cube)

    res <- c(sf::st_intersects(extents, geom, sparse = FALSE) & !sf::st_touches(extents, geom, sparse = FALSE))

    if (length(res) == 0) {

        warning(sprintf("No tile intersects the informed geometry."), call. = FALSE)
        invisible(NULL)
    }

    return(res)
}


#' @title Cubes management functions
#'
#' @name cube_tiles
#'
#' @description Lists all registered tiles in a cube.
#'
#' @param cube   A \code{EOCubes_cube} data structure.
#' @param which   A \code{logical} or \code{integer} vector indicating which tile to retrieve.
#' @param prefix   A \code{character} vector containing tile name prefix to be filtered.
#'
#' @return A \code{list} of tiles in a cube or \code{NULL} if filter does not return tiles.
#'
#' @details
#' The \code{which} vector must have the same length as cube tiles list. It can
#' be used with \code{sf} dependent function \code{tiles_intersects} to retrieve
#' only a subset of tiles that intersects some geometry.
#'
#' @export
#'
cube_tiles <- function(cube, which = NULL, prefix = NULL) {

    if (!"EOCubes_cube" %in% class(cube))
        stop("You must inform a `EOCubes_cube` as data input.")

    if (is.null(cube$tiles))
        invisible(NULL)

    if (is.null(which) && is.null(prefix)) {

        res <- structure(cube$tiles, class = "EOCubes_tilelist")
        return(res)
    }

    res <- .filter_prefix(prefix, cube$tiles)

    if (length(res) == 0) {

        warning(sprintf("No tile with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    if (!is.null(which)) {

        if (!(is.logical(which) && length(which) == length(cube$tiles)) &&
            !(is.integer(which)))
            stop("`which` parameter must have the same length as cube's tiles list")

        res <- cube$tiles[which]

        if (length(res) == 0) {

            warning(sprintf("No tile returned."), call. = FALSE)
            invisible(NULL)
        }
    }

    res <- structure(res, class = "EOCubes_tilelist")
    return(res)
}

#' @title Cubes management functions
#'
#' @name filter_tiles
#'
#' @description Returns a new \code{EOCubes_cube} data structure with filtered tiles and
#' fetches tiles data from remote.
#'
#' @param cube   A \code{EOCubes_cube} data structure.
#' @param which   A \code{logical} or \code{integer} vector indicating which tile to retrieve.
#' @param prefix   A \code{character} vector containing tile name prefix to be filtered.
#'
#' @return A \code{EOCubes_fetched, EOCubes_cube} cube data structure.
#'
#' @details
#' The \code{which} vector must have the same length as cube tiles list. It can
#' be used with \code{sf} dependent function \code{tiles_intersects} to retrieve
#' only a subset of tiles that intersects some geometry.
#'
#' The returned cube object \code{EOCubes_cube} is a fetched cube \code{EOCubes_fetched},
#' that means that all bands and rasters references are in memory.
#'
#' @export
#'
filter_tiles <- function(cube, which = NULL, prefix = NULL) {

    if (!"EOCubes_cube" %in% class(cube))
        stop("You must inform a `EOCubes_cube` as data input.")

    if (is.null(cube$tiles))
        return(cube)

    if (is.null(which) && is.null(prefix)) {

        return(cube)
    }

    tiles <- cube_tiles(cube = cube, which = which, prefix = prefix)
    tiles <- .fetch_tiles(tiles)

    # merge layers
    if (!is.null(tiles))
        tiles <- lapply(tiles, function(x) {
            lapply(x$bands, function(y) {

                do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), y$layers))
            })
        })

    cube$tiles <- tiles

    class(cube) <- c("EOCubes_fetched", "EOCubes_cube")
    return(cube)
}
