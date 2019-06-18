#### config catalog ####

check.eo_config <- function(cf) {

    if (is.null(cf$remotes) || is.null(names(cf$remotes)) ||
        any(sapply(cf$remotes, function(x) (is.null(x$href) || is.null(x$description)))))
        stop("Invalid config file definition.", call. = FALSE)

    return(cf)
}

add_item.eo_config <- function(cf, pr, name) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    if (exists_item(cf, name))
        stop(sprintf("Provider entry '%s' already exists.", name), call. = FALSE)

    cf$remotes[[name]] <- as_entry(pr)

    return(cf)
}

del_item.eo_config <- function(cf, name) {

    if (!exists_item(cf, name))
        stop(sprintf("Provider entry '%s' not found.", name), call. = FALSE)

    cf$remotes[[name]] <- NULL

    return(cf)
}

list_items.eo_config <- function(cf) {

    return(names(cf$remotes))
}

exists_item.eo_config <- function(cf, name) {

    return(all(name %in% names(cf$remotes)))
}

get_item.eo_config <- function(cf, name) {

    if (!exists_item(cf, name))
        stop(sprintf("Provider entry '%s' not found.", name), call. = FALSE)

    return(cf$remotes[[name]])
}
