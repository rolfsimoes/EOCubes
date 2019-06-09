#' @title Cube functions
#'
#' @name cube_functions
#'
#' @description These functions provides a basic operations over cubes.
#' A cube is an entry point a repository that can maintain many cubes.
#'
#' @param x   A \code{list} data structure to be converted
#' to \code{EO_cube} object.
#' @param prefix   A \code{character} containing cube/cube entry name
#' prefix to be filtered.
#' @param name   A \code{character} text with cube name.
#' @param caching   A \code{logical} value indicating wether to use cache system.
#' @param repos   An \code{EO_cube} object.
#'
#' @seealso \code{\link{list_cubes}}
#'
#' @examples
#' list_cubes()
#' x <- cube("localhost")
#' cube_name(x)   # shows 'localhost'
#' list_cubes(x)   # list cubes in 'localhost'
#'
NULL

#' @describeIn cube_functions Define the meta-data of a \code{EO_cube}.
#'
#' @return A \code{meta_cube} object.
#'
#' @export
#'
cube_crs <- function(x, ...) {

    UseMethod("cube_crs")
}

cube_crs.EO_cube <- function(x) {

    res <- x$crs

    return(res)
}

#' @describeIn cube_functions Create a new \code{bbox_cube} object.
#'
#' @return A \code{bbox_cube} object.
#'
#' @export
#'
bbox <- function(xmin, ymin, xmax, ymax) {

    if (any(c(!is.numeric(xmin), !is.numeric(ymin),
              !is.numeric(xmax), !is.numeric(ymax))))
        stop("Invalid numeric value as bbox parameter.", call. = FALSE)

    res <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    class(res) <- "bbox_cube"

    return(res)
}

#' @describeIn cube_functions Create a new \code{interval_cube} object.
#'
#' @return An \code{interval_cube} object.
#'
#' @export
#'
interval <- function(from, to) {

    if (any(c(is.na(from <- as.Date(from[[1]], "%Y-%m-%d")),
              is.na(to <- as.Date(to[[1]], "%Y-%m-%d")))))
        stop("Invalid date value in interval parameter.")

    res <- c(from = from, to = to)
    class(res) <- "interval_cube"

    return(res)
}

#' @describeIn cube_functions Create a new \code{band_cube} object.
#'
#' @return A \code{band_cube} object.
#'
#' @export
#'
band <- function(name, min, max, fill, scale) {

    res <- structure(list(min = min, max = max, fill = fill,
                          scale = scale), band_name = name, class = "band_cube")

    return(res)
}

#' @describeIn cube_functions Create a new \code{band_cube_list} object.
#'
#' @return A \code{band_cube_list} object.
#'
#' @export
#'
bands <- function(x) {

    if (!all(sapply(x, inherits, what = "band_cube")))
        stop("Invalid list of `band_cube`.", call. = FALSE)

    names(x) <- sapply(x, attr, which = "band_name", exact = TRUE)
    res <- structure(x, class = "band_cube_list")

    return(res)
}

#' @describeIn cube_functions Create a new \code{EO_cube} object.
#'
#' @return An \code{EO_cube} object.
#'
#' @export
#'
new_cube <- function(name, description, keywords, crs, bbox, interval,
                     bands, items = list()) {

    if (any(c(!is.atomic(description), !is.atomic(keywords), !is.atomic(crs),
              !inherits(bbox, "bbox_cube"), !inherits(interval, "interval_cube"),
              !inherits(bands, "bands_cube_list"))))
        stop("Invalid cube parameter values.", call. = FALSE)

    res <- new_catalog(name = name, type = "cube", version = "0.8",
                       id = name, description = description, keywords = keywords,
                       crs = crs, extent = extent, interval = interval,
                       bands = bands, items = items)

    return(res)
}

#' @describeIn cube_functions Fetches a registered cube.
#'
#' @return An \code{EO_cube} object.
#'
#' @details The function \code{cube} fetches the cube registered on a given
#' entry \code{name}.
#'
#' @export
#'
cube <- function(name, prov = provider("localhost")) {

    res <- open_item(x = prov, name = name)

    return(res)
}

#' @describeIn cube_functions Returns the name of a cube.
#'
#' @return A \code{character}.
#'
#' @details The function \code{cube_name} show the entry name of the cube
#' list from which the \code{EO_cube} object have been fetched.
#'
#' @export
#'
cube_name <- function(x, ...) {

    UseMethod("cube_name")
}

#' @method cube_name EO_cube
#'
#' @export
#'
cube_name.EO_cube <- function(x) {

    res <- catalog_name(x)

    return(res)
}

#' @describeIn cube_functions Save a cube definition in a file.
#'
#' @return None.
#'
#' @export
#'
save_cube <- function(x, ...) {

    UseMethod("save_cube")
}

#' @method save_cube EO_cube
#'
#' @export
#'
save_cube.EO_cube <- function(x, file = NULL) {

    save_catalog(x = x, file = file)
    invisible(NULL)
}

#' @describeIn cube_functions Load a cube definition from a location.
#'
#' @return An \code{EO_cube} object.
#'
#' @export
#'
load_cube <- function(...) {

    UseMethod("load_cube")
}

#' @method load_cube EO_cube
#'
#' @export
#'
load_cube.EO_cube <- function(location, name, cache = NULL) {

    res <- load_catalog(location = location, name = name, cache = cache)

    return(res)
}

#' @describeIn cube_functions Append a tile reference to a cube.
#'
#' @return An \code{EO_cube} object.
#'
#' @export
#'
link_tile <- function(x = cube("localhost"), ...) {

    UseMethod("link_tile")
}

#' @method link_tile EO_cube
#'
#' @export
#'
link_tile.EO_cube <- function(x, name, location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a JSON file location.")

    res <- link_item(x = x, name = name, location = location, require_type = "EO_cube")

    return(res)
}

#' @describeIn cube_functions Remove a tile reference from a cube.
#'
#' @return An \code{EO_cube} object.
#'
#' @export
#'
unlink_tile <- function(x, ...) {

    UseMethod("unlink_tile")
}

#' @method unlink_tile EO_cube
#'
#' @export
#'
unlink_tile.EO_cube <- function(x, name) {

    res <- unlink_item(x = x, name = name)

    return(res)
}

#' @describeIn cube_functions List all registered tiles in a cube.
#'
#' @return An \code{EO_cube_entries} object.
#'
#' @details The function \code{list_tiles} lists all registered tiles in a
#' cube. Use \code{prefix} parameter to filter the entries by name.
#'
#' @export
#'
list_tiles <- function(x, ...) {

    UseMethod("list_tiles")
}

#' @method list_tiles EO_cube
#'
#' @export
#'
list_tiles.EO_cube <- function(x, prefix = NULL) {

    res <- list_items(x = x, prefix = prefix)

    return(res)
}

as_entry.EO_cube <- function(x, location) {

    res <- list(description = x$description,
                href = location)

    return(res)
}

cast_catalog.EO_cube_0.8 <- function(x) {

    if (any(c(is.null(x$id), is.null(x$description), is.null(x$keywords),
              is.null(x$crs), is.null(x$extent), is.null(x$interval),
              is.null(x$bands), is.null(x$items))))
        stop("Invalid cube data definition.", call. = FALSE)

    return(x)
}
