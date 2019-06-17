
#### provider entry ####

as_entry.eo_provider <- function(pr) {

    res <- list(href = reference(pr), description = description(pr))

    return(res)
}

describe_entry.eo_provider <- function(en) {

    return(en$description)
}

check_entry.eo_provider <- function(en) {

    if (is.null(en$href) || is.null(en$description))
        stop("Invalid entry.", call. = FALSE)

    invisible(NULL)
}

open_entry.eo_provider <- function(en) {

    con <- new_connection(en$href)

    res <- provider(con)

    close(con)

    return(res)
}

#### provider catalog ####

as_list.eo_provider <- function(pr) {

    pr$cubes <- as.list(pr$cubes)

    return(pr)
}

check.eo_provider <- function(pr) {

    if (is.null(pr$description) || is.null(pr$cubes) || is.null(names(pr$cubes)) ||
        any(sapply(pr$cubes, function(x) (is.null(x$href) || is.null(x$description)))))
        stop("Invalid provider file definition.", call. = FALSE)

    pr$cubes <- as.environment(pr$cubes)

    return(pr)
}

description.eo_provider <- function(pr) {

    return(pr$description)
}

add_item.eo_provider <- function(pr, cb, name) {

    if (!inherits(cb, "eo_cube"))
        stop("Invalid cube.", call. = FALSE)

    if (exists_item(pr, name = name))
        stop(sprintf("Cube entry '%s' already exists.", name), call. = FALSE)

    pr$cubes[[name]] <- as_entry(cb)

    invisible(NULL)
}

del_item.eo_provider <- function(pr, name) {

    if (!exists_item(pr, name = name))
        stop(sprintf("Cube entry '%s' not found.", name), call. = FALSE)

    pr$cubes[[name]] <- NULL

    invisible(NULL)
}

list_items.eo_provider <- function(pr) {

    return(names(pr$cubes))
}

exists_item.eo_provider <- function(pr, name) {

    return(all(name %in% names(pr$cubes)))
}
