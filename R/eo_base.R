#### functions ####

new_connection <- function(path) {

    res <- tryCatch(
        suppressWarnings(file(path)),
        error = function(e)
            stop(sprintf(paste("Invalid file location '%s'.",
                               "Reported error: %s"), path, e$message), call. = FALSE))

    return(res)
}

open_json <- function(con) {

    txt <- tryCatch(
        suppressWarnings(readLines(con, warn = FALSE)),
        error = function(e)
            stop(sprintf(paste("Error while opening JSON from connection.",
                               "Reported error: %s"), e$message), call. = FALSE))

    res <- jsonlite::fromJSON(txt,
                              simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE)

    reference(res) <- summary(con)[["description"]]

    return(res)
}

save_json <- function(x, con) {

    txt <- jsonlite::toJSON(as_list(x), pretty = TRUE, auto_unbox = TRUE)

    tryCatch(
        suppressWarnings(writeLines(txt, con, useBytes = TRUE)),
        error = function(e)
            stop(sprintf(paste("Error while saving JSON file.",
                               "Reported error: %s"), e$message), call. = FALSE))

    invisible(NULL)
}

ifnull <- function(x, value) {

    if (is.null(x))
        return(value)

    return(x)
}

reference <- function(pr) {

    return(attr(pr, "reference", TRUE))
}

`reference<-` <- function(pr, value) {

    tryCatch(close(new_connection(value)),
             error = function(e)
                 stop(sprintf(paste("Invalid `reference` '%s'.",
                                    "Reported error: %s"), value, e$message), call. = FALSE))

    attr(pr, "reference", TRUE) <- value

    invisible(NULL)
}

bbox <- function(x, crs) {

    x <- unlist(x, use.names = FALSE)

    if (!is.numeric(x) || any(is.na(x)) || length(x) != 4)
        stop("Invalid bbox value.", call. = FALSE)

    res <- c(xmin = x[[1]], ymin = x[[2]], xmax = x[[3]], ymax = x[[4]])
    class(res) <- "bbox"
    attr(res, "crs", TRUE) <- crs

    return(res)
}

bbox_as_sfc <- function(b) {

    if (!requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (!inherits(b, "bbox"))
        stop("Invalid bbox value.", call. = FALSE)

    res <- sf::st_sfc(list(sf::st_polygon(
        list(matrix(b[c(1,2,1,4,3,4,3,2,1,2)], ncol = 2, byrow = T)))),
        crs = attr(b, "crs", TRUE))

    return(res)
}

intersects <- function(x1, x2) {

    if ((inherits(x1, c("sf", "sfc")) || inherits(x2, c("sf", "sfc"))) &&
        !requireNamespace("sf", quietly = TRUE))
        stop("You need `sf` package to run this function.", call. = FALSE)

    if (inherits(x1, "bbox")) {

        if (inherits(x2, "bbox")) {

            x_between <-
                (x2[[1]] <= x1[[1]] && x1[[1]] <= x2[[3]]) ||
                (x2[[1]] <= x1[[3]] && x1[[3]] <= x2[[3]]) ||
                (x1[[1]] <= x2[[1]] && x2[[1]] <= x1[[3]]) ||
                (x1[[1]] <= x2[[3]] && x2[[3]] <= x1[[3]])

            y_between <-
                (x2[[2]] <= x1[[2]] && x1[[2]] <= x2[[4]]) ||
                (x2[[2]] <= x1[[4]] && x1[[4]] <= x2[[4]]) ||
                (x1[[2]] <= x2[[2]] && x2[[2]] <= x1[[4]]) ||
                (x1[[2]] <= x2[[4]] && x2[[4]] <= x1[[4]])

            return(x_between && y_between)

        } else if (inherits(x2, c("sf", "sfc"))) {

            x1 <- bbox_as_sfc(x1, crs = crs)
            geom <- sf::st_transform(geom, crs = cube_crs(cube = cube))

            sfc <- tiles_sfc(cube = cube)

            selected <- selected & c(sf::st_intersects(sfc, geom, sparse = FALSE) & !sf::st_touches(sfc, geom, sparse = FALSE))
        } else
            stop()

    } else if (inherits(x1, c("sf", "sfc"))) {

        if (inherits(x2, "bbox")) {

        } else if (inherits(x2, c("sf", "sfc"))) {

        } else
            stop()
    }
}

interval <- function(from, to) {

    if (!is.null(from) && (length(from) != 1 || is.na(from <- as.Date(from))))
        stop("Invalid `from` date.", call. = FALSE)

    if (!is.null(to) && (length(to) != 1 || is.na(to <- as.Date(to))))
        stop("Invalid `to` date.", call. = FALSE)

    res <- list(from = from, to = to)
    class(res) <- "interval"

    return(res)
}

slices <- function(start, time_length, step_period = "12 months") {

    if (is.null(start) || length(start) != 1 || is.na(as.Date(start)))
        stop("Invalid `start` date.", call. = FALSE)

    if (!is.numeric(time_length) || length(time_length) != 1 ||
        is.na(time_length) || time_length <= 0)
        stop("Invalid `time_length` value.", call. = FALSE)

    tryCatch(seq(start, by = step_period, length.out = 2),
             error = function(e) stop("Invalid `step_period` string.", call. = FALSE))

    res <- list(start = start, time_length = time_length,
                step_period = step_period)
    class(res) <- "slices"

    return(res)
}

cast <- function(x, type) {

    version <- ifnull(x$version, supported_versions()[[1]])

    if (all(supported_versions() != version))
        stop("The catalog version is not supported.", call. = FALSE)

    x$version <- version

    type <- paste(type, c(version, ""), sep = "_")

    res <- check(structure(x, class = type))

    return(res)
}

cast_entry <- function(x, default_type) {

    type <- ifnull(x$type, default_type)

    res <- check_entry(structure(x, class = type))

    return(res)
}

#### entry ####

# Provide the capability to be an item of some catalog
as_entry <- function(...) {

    UseMethod("as_entry")
}

# Show a description of some entry type
describe_entry <- function(...) {

    UseMethod("describe_entry")
}

# [>=0.8] Check for data consistency of some entry type
check_entry <- function(...) {

    UseMethod("check_entry")
}

# [>=0.8] Find the correct way to open an entry of some type
open_entry <- function(...) {

    UseMethod("open_entry")
}

#### catalog ####

# >>catalog
# as_list
# check
# description
# add_entry
# del_entry
# list_entries
# as_entry
#
# >>entry
# describe_entry
# check_entry
# open_entry
#

# convert the object to a list data
as_list <- function(...) {

    UseMethod("as_list")
}

as_list.default <- function(x) {

    class(x) <- NULL

    return(x)
}

# Check for the consistency of the internal data
check <- function(...) {

    UseMethod("check")
}

# Get a description of the catalog
description <- function(...) {

    UseMethod("description")
}

# Provide the capability to add items
add_item <- function(...) {

    UseMethod("link")
}

# Provide the capability to remove items
del_item <- function(...) {

    UseMethod("unlink")
}

# Provide the capability to list the items of the catalog
list_items <- function(...) {

    UseMethod("list_items")
}

# Verify if an item exists
exists_item <- function(...) {

    UseMethod("exists_item")
}
