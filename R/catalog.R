#' @title Catalog functions
#'
#' @name catalog_functions
#'
#' @param x   A data structure.
#' @param name   A \code{character} text with catalog name.
#' @param caching   A \code{logical} value indicating wether to use cache system.
#'
#' @description These functions provides the basic operations over catalogs
#' objects.
#'
NULL

make_class_name <- function(type, version) {

    res <- c(paste("EO", res$type, res$version, sep = "_"),
             paste("EO", res$type, sep = "_"))

    return(res)
}

new_catalog <- function(name, type, version, ..., items = list()) {

    res <- c(list(type = type,
                  version = version),
             list(...))
    if (!is.null(items))
        res <- c(res, list(items = items))

    res <- structure(res,
                     catalog_name = name,
                     class = make_class_name(type = type, version = version))

    return(res)
}

load_catalog <- function(location, name, cache = NULL) {

    if (!is.null(cache) && exists(location, .cache, inherits = FALSE) &&
        .cache[[location]]$cache == cache)
        return(.cache[[location]]$content)

    con <- tryCatch(file(location),
                    error = function(e) {
                        stop(sprintf("Invalid file location '%s'", location), call. = FALSE)
                    })
    writable <- (summary(con)$`can write` == "yes")

    res <- tryCatch(
        jsonlite::fromJSON(suppressWarnings(readLines(con, warn = FALSE)),
                           simplifyDataFrame = FALSE,
                           simplifyMatrix = FALSE),
        error = function(e) {

            stop(sprintf(paste("Error while opening JSON from '%s'.",
                               "Reported error: %s"), location, e$message), call. = FALSE)
        }, finally = close(con))

    if (!is.null(cache) && .cache[[location]]$cache != cache)
        .cache[[location]] <- list(content = res, cache = cache, timestamp = Sys.time())

    if (any(c(is.null(res$type), is.null(res$version), is.null(res$items))))
        stop("Invalid data definition.", call. = FALSE)

    res <- structure(res,
                     catalog_name = name,
                     class = make_class_name(res$type, res$version))

    res <- tryCatch(cast_catalog(res),
                    error = function(e) stop("The version of the data definition is not supported.", call. = FALSE))

    if (writable)
        attr(res, "writable_location", TRUE) <- location

    return(res)
}

get_writable_location <- function(x) {

    res <- attr(x, "writable_location", TRUE)

    return(res)
}

cast_catalog <- function(x, ...) {

    if (any(c(is.null(x$type), is.null(x$version))))
        stop("Invalid data definition.", call. = FALSE)

    UseMethod("cast_catalog")
}

as_entry <- function(x, ...) {

    UseMethod("as_entry")
}

catalog_name <- function(x, ...) {

    UseMethod("catalog_name")
}

catalog_name.default <- function(x) {

    res <- attr(x, "catalog_name", TRUE)

    return(res)
}

link_item <- function(x, ...) {

    UseMethod("link_item")
}

link_item.default <- function(x, name, location, required_type) {

    if (name %in% names(x$items))
        stop(sprintf("The catalog '%s' already exists.", name), call. = FALSE)

    obj <- load_catalog(location = location, name = name)


    if (!inherits(obj, required_type))
        stop(sprintf("The location '%s' is not a valid '%s' definition.", location, required_type),
             call. = FALSE)

    x$items[[name]] <- as_entry(x = obj, location = location,
                                cache = substring(text = tempfile("", ""), first = 2))

    return(x)
}

unlink_item <- function(x, ...) {

    UseMethod("unlink_item")
}

unlink_item.default <- function(x, name) {

    if (!(name %in% names(x$items)))
        stop(sprintf("The entry '%s' does not exist.", name), call. = FALSE)

    x$items[[name]] <- NULL

    return(x)
}

list_items <- function(x, ...) {

    UseMethod("list_items")
}

list_items.default <- function(x, prefix) {

    if (length(x$items) == 0) {

        warning("No entries.", call. = FALSE)
        invisible(NULL)
    }

    if (is.null(prefix)) {

        res <- structure(x$items, class = paste(class(x)[[1]], "entries", sep = "_"))
        return(res)
    }

    selected <- .select_prefix(prefix, x$items)

    if (!any(selected)) {

        warning(sprintf("No entries with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(x$items[selected], class = paste(class(x)[[1]], "entries", sep = "_"))
    return(res)
}

save_catalog <- function(x, ...) {

    UseMethod("save_catalog")
}

save_catalog.default <- function(x, file) {

    if (is.null(file))
        file <- get_writable_location(x)

    if (is.null(file))
        stop("The data definition is not writable", call. = FALSE)

    tryCatch(
        suppressWarnings(jsonlite::write_json(x = c(x),
                                              path = file,
                                              pretty = TRUE,
                                              auto_unbox = TRUE)),
        error = function(e) {

            stop(sprintf(paste("Error while saving JSON file '%s'.",
                               "Reported error: %s"), file, e$message), call. = FALSE)
        })

    invisible(NULL)
}

open_item <- function(x, ...) {

    UseMethod("open_item")
}

open_item.default <- function(x, name) {

    res <- load_catalog(location = x$item[[name]]$href, name = name,
                        cache = x$item[[name]]$cache)

    return(res)
}


cast_catalog.EO_tile_0.8 <- function(x) {

    if (any(c(is.null(x$id), is.null(x$description), is.null(x$keywords),
              is.null(x$extent), is.null(x$meta))))
        stop("Invalid tile data definition.", call. = FALSE)

    return(x)
}

as_entry.EO_cube_0.8 <- function(x, location) {

    res <- list(description = x$description,
                href = location)

    return(res)
}

as_entry.EO_tile_0.8 <- function(x, location) {

    res <- list(href = location,
                extent = get_extent(x))

    return(res)
}
