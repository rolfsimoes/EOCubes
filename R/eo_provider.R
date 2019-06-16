provider <- function(...) {

    UseMethod("provider")
}

provider.connection <- function(con) {

    res <- cast(open_json(con), default_type = "eo_provider")

    return(res)
}

provider.character <- function(name) {

    res <- provider(.global[["conf"]], name = name)

    return(res)
}

add_cube <- function(pr, cb) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    add_item(pr, cb, name(cb))

    invisible(NULL)
}

del_cube <- function(pr, name) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    del_item(pr, name = name)

    invisible(NULL)
}

list_cubes <- function(pr) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    return(list_items(pr))
}
