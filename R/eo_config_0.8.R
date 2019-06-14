
cast.eo_config_0.8 <- function(cf) {

    if ((cf$type != "#config") || is.null(cf$items) || is.null(names(pr$items)) ||
        any(sapply(cf$items, function(x) (is.null(x$href) || is.null(x$description) || is.null(x$type)))))
        stop("Invalid config file definition.", call. = FALSE)

    # criar um handler para o type

    return(cf)
}

link.eo_config_0.8 <- function(cf, pr, name) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    if (name %in% names(cf$items))
        stop(sprintf("Provider entry '%s' already exists.", name), call. = FALSE)

    cf$items[[name]] <- entry(pr)

    invisible(NULL)
}

unlink.eo_config_0.8 <- function(cf, name) {

    if (!name %in% names(cf$items))
        stop(sprintf("Provider entry '%s' not found.", name), call. = FALSE)

    cf$items[[name]] <- NULL

    invisible(NULL)
}

provider.eo_config_0.8 <- function(cf, name) {

    if (!name %in% names(cf$items))
        stop(sprintf("Provider entry '%s' not found.", name), call. = FALSE)

    res <- open_entry(cf$items[[name]])

    return(res)
}

list_providers.eo_config_0.8 <- function(cf) {

    return(names(cf$items))
}
