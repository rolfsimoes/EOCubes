
cast.eo_provider_0.7 <- function(pr) {

    if (is.null(pr$description) || is.null(pr$cubes) || is.null(names(pr$cubes)) ||
        any(sapply(pr$cubes, function(x) (is.null(x$href) || is.null(x$description)))))
        stop("Invalid provider file definition.", call. = FALSE)

    return(pr)
}

entry.eo_provider_0.7 <- function(pr) {

    res <- list(type = class(pr), href = reference(pr), description = description(pr))

    return(res)
}

open_entry.eo_provider_0.7 <- function(en) {

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

link.eo_provider_0.7 <- function(pr, cb, name) {

    if (!inherits(cb, "eo_cube"))
        stop("Invalid cube.", call. = FALSE)

    if (name %in% names(pr$cubes))
        stop(sprintf("Cube entry '%s' already exists.", name), call. = FALSE)

    pr$cubes[[name]] <- list(href = reference(cb), description = description(cb))

    invisible(NULL)
}

list_cubes.eo_provider_0.7 <- function(pr) {

    return(names(pr$cubes))
}

cube.eo_provider_0.7 <- function(pr = provider("localhost"),
                                 name, select_bands = NULL,
                                 interval_from = NULL, interval_to = NULL,
                                 in_geometry = NULL, select_tiles = NULL) {


    if (!name %in% names(pr$cubes))
        stop(sprintf("Cube entry '%s' not found.", name), call. = FALSE)

    href <- pr$cubes[[name]]$href

    con <- tryCatch(
        suppressWarnings(file(href)),
        error = function(e) {

            stop(sprintf(paste("Invalid file location '%s'.",
                               "Reported error: %s"), href, e$message), call. = FALSE)
        })

    res <- cube(con, select_bands = select_bands,
                interval_from = interval_from, interval_to = interval_to,
                in_geometry = in_geometry, select_tiles = select_tiles)

    close(con)

    return(res)
}

description.eo_provider_0.7 <- function(pr) {

    return(pr$description)
}

type.eo_provider_0.7 <- function(pr) {

    return("eo_provider")
}
