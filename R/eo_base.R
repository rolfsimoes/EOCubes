
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

    return(res)
}

load_config <- function(...) {

    UseMethod("load_config")
}

load_config.connection <- function(con) {

    .global[["conf"]] <- new_object(open_json(con), type = "config", res$version)

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
