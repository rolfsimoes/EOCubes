#### functions ####

new_connection <- function(path) {

    res <- tryCatch(
        suppressWarnings(file(path)),
        error = function(e)
            stop(sprintf(paste("Invalid file location '%s'.",
                               "Reported error: %s"), path, e$message), call. = FALSE))

    return(res)
}

new_eo_object <- function(x, type) {

    if (!type %in% names(supported_versions()))
        stop("The catalog `type` is not supported.", call. = FALSE)

    version <- ifnull(x$version, supported_versions(type)[[1]])

    if (all(supported_versions(type) != version))
        stop("The catalog `version` is not supported.", call. = FALSE)

    res <- check(structure(x, class = paste(type, c(version, ""), sep = "_")))

    attr(res, "context", TRUE) <- new.env()

    return(res)
}

open_eo_object <- function(con, type) {

    must_close <- !isOpen(con)

    txt <- tryCatch(
        suppressWarnings(readLines(con, warn = FALSE)),
        error = function(e)
            stop(sprintf(paste("Error while opening JSON from connection.",
                               "Reported error: %s"), e$message), call. = FALSE))

    res <- jsonlite::fromJSON(txt,
                              simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE)

    res <- new_eo_object(res, type = type)

    reference(res) <- summary(con)[["description"]]

    if (must_close)
        close(con)

    return(res)
}

save_eo_object <- function(x, con) {

    must_close <- !isOpen(con)

    txt <- jsonlite::toJSON(as_list(x), pretty = TRUE, auto_unbox = TRUE)

    tryCatch(
        suppressWarnings(writeLines(txt, con, useBytes = TRUE)),
        error = function(e)
            stop(sprintf(paste("Error while saving JSON file.",
                               "Reported error: %s"), e$message), call. = FALSE))

    reference(x) <- summary(con)[["description"]]

    if (must_close)
        close(con)

    invisible(NULL)
}

reference <- function(x) {

    return(attr(x, "context", TRUE)[["reference"]])
}

`reference<-` <- function(x, value) {

    tryCatch(close(new_connection(value)),
             error = function(e)
                 stop(sprintf(paste("Invalid `reference` '%s'.",
                                    "Reported error: %s"), value, e$message), call. = FALSE))

    attr(x, "context", TRUE)[["reference"]] <- value

    invisible(NULL)
}

ifnull <- function(x, value) {

    if (is.null(x))
        return(value)

    return(x)
}

# cast <- function(x, type) {
#
#     if (!type %in% names(supported_versions()))
#         stop("The catalog `type` is not supported.", call. = FALSE)
#
#     version <- ifnull(x$version, supported_versions(type)[[1]])
#
#     if (all(supported_versions(type) != version))
#         stop("The catalog `version` is not supported.", call. = FALSE)
#
#     res <- check(structure(x, class = paste(type, c(version, ""), sep = "_")))
#
#     return(res)
# }
#
cast_entry <- function(x, default_type) {

    type <- ifnull(x$type, default_type)

    if (!type %in% names(supported_versions()))
        stop("The catalog `type` is not supported.", call. = FALSE)

    version <- ifnull(x$version, supported_versions(type)[[1]])

    if (all(supported_versions(type) != version))
        stop("The catalog `version` is not supported.", call. = FALSE)

    res <- check_entry(structure(x, class = paste(type, c(version, ""), sep = "_")))

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

# Check for data consistency of some entry type
check_entry <- function(...) {

    UseMethod("check_entry")
}

# Find the correct way to open an entry of some type
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

    UseMethod("add_item")
}

# Provide the capability to remove items
del_item <- function(...) {

    UseMethod("del_item")
}

# Provide the capability to list the items of the catalog
list_items <- function(...) {

    UseMethod("list_items")
}

# Verify if an item exists
exists_item <- function(...) {

    UseMethod("exists_item")
}

# Verify if an item exists
get_item <- function(...) {

    UseMethod("get_item")
}
