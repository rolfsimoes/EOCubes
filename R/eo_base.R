
open_json <- function(...) {

    UseMethod("open_json")
}

open_json.connection <- function(con) {

    txt <- tryCatch(
        suppressWarnings(readLines(con, warn = FALSE)),
        error = function(e) {

            stop(sprintf(paste("Error while opening JSON from connection.",
                               "Reported error: %s"), e$message), call. = FALSE)
        })

    res <- jsonlite::fromJSON(txt,
                              simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE)

    attr(res, "reference") <- summary(con)[["description"]]

    return(res)
}

load_config <- function(...) {

    UseMethod("load_config")
}

load_config.connection <- function(con) {

    .global[["conf"]] <- new_object(open_json(con), type = "eo_config")

    invisible(NULL)
}

save_config <- function(...) {

    UseMethod("save_config")
}

save_config.connection <- function(con) {

    txt <- jsonlite::toJSON(as_list(.global[["conf"]]),
                            pretty = TRUE, auto_unbox = TRUE)

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

    return(attr(pr, "reference"))
}

#### catalog ####

# Detect the type on an entry
entry_type <- function(en, default_type) {

    type <- en$type

    if (is.null(en$type))
        type <- default_type

    res <- check_entry(new_object(en, type = type))

    return(res)
}

# Provide the capability to add items
link <- function(...) {

    UseMethod("link")
}

# Provide the capability to remove items
unlink <- function(...) {

    UseMethod("unlink")
}

# Provide capability to be an item of some catalog
entry <- function(...) {

    UseMethod("entry")
}

# Check for data consistency of some entry type
check_entry <- function(...) {

    UseMethod("check_entry")
}

# Show some description of some entry type
describe_entry <- function(...) {

    UseMethod("check_entry")
}

# Find the correct way to open an entry of some type
open_entry <- function(...) {

    UseMethod("open_entry")
}

open_entry.default <- function(en, default_type) {

    res <- open_entry(entry_type(en, default_type = default_type))

    return(res)
}
