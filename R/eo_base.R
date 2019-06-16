#### functions ####

open_json <- function(con) {

    txt <- tryCatch(
        suppressWarnings(readLines(con, warn = FALSE)),
        error = function(e) {

            stop(sprintf(paste("Error while opening JSON from connection.",
                               "Reported error: %s"), e$message), call. = FALSE)
        })

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
        error = function(e) {

            stop(sprintf(paste("Error while saving JSON file.",
                               "Reported error: %s"), e$message), call. = FALSE)
        })

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

    attr(pr, "reference", TRUE) <- value

    invisible(NULL)
}

bbox.default <- function(x) {

    if (length(x1) != 4)
        stop("Invalid bbox parameters.", call. = FALSE)

    res <- c(xmin = x[[1]], ymin = x[[2]], xmax = x[[3]], ymax = x[[4]])
    class(res) <- "bbox"

    return(res)
}

interval.default <- function(from, to) {

    res <- list(from = from, to = to)
    class(res) <- "interval"

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

