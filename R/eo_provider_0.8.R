cast.eo_provider_0.8 <- function(pr) {

    if ((pr$type != "eo_provider_0.8") || is.null(pr$description) || is.null(pr$items) ||
        is.null(names(pr$items)) ||
        any(sapply(pr$items, function(x) (is.null(x$type) || is.null(x$href)))))
        stop("Invalid provider file definition.", call. = FALSE)

    return(pr)
}

entry.eo_provider_0.8 <- function(pr) {

    res <- list(type = class(pr), href = reference(pr), description = description(pr),
                class = class(pr))

    return(res)
}

open_entry.eo_provider_0.8 <- function(en) {

    con <- tryCatch(
        suppressWarnings(file(en$href)),
        error = function(e) {

            stop(sprintf(paste("Invalid file location '%s'.",
                               "Reported error: %s"), en$href, e$message), call. = FALSE)
        })

    res <- provider(con)

    close(con)

    return(res)
}

link.eo_provider_0.8 <- function(pr, cb, name) {

    if (!inherits(cb, "eo_cube"))
        stop("Invalid cube.", call. = FALSE)

    if (name %in% names(pr$items))
        stop(sprintf("Cube entry '%s' already exists.", name), call. = FALSE)

    pr$items[[name]] <- entry(cb)

    invisible(NULL)
}

list_cubes.eo_provider_0.8 <- function(pr) {


}


cube.eo_provider_0.8 <- function(pr = provider("localhost"),
                                 name, select_bands = NULL,
                                 interval_from = NULL, interval_to = NULL,
                                 in_geometry = NULL, select_tiles = NULL) {

    if (!name %in% names(pr$items))
        stop(sprintf("Cube entry '%s' not found.", name), call. = FALSE)

    res <- open_entry(pr$items[[name]])

    return(res)
}

description.eo_provider_0.8 <- function(pr) {

    return(pr$description)
}

type.eo_provider_0.8 <- function(pr) {

    return(pr$type)
}
