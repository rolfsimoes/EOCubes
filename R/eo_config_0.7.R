
cast.eo_config_0.7 <- function(cf) {

    if (is.null(cf$remotes) || is.null(names(pr$remotes)) ||
        any(sapply(cf$remotes, function(x) (is.null(x$href) || is.null(x$description)))))
        stop("Invalid config file definition.", call. = FALSE)

    return(cf)
}

link.eo_config_0.7 <- function(cf, pr, name) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    if (name %in% names(cf$remotes))
        stop(sprintf("Provider entry '%s' already exists.", name), call. = FALSE)

    cf$remotes[[name]] <- list(href = reference(pr), description = description(pr))

    invisible(NULL)
}

unlink.eo_config_0.7 <- function(cf, name) {

    if (!name %in% names(cf$remotes))
        stop(sprintf("Provider entry '%s' not found.", name), call. = FALSE)

    cf$remotes[[name]] <- NULL

    invisible(NULL)
}

open_entry.entry_eo_config_0.7 <- function(cf, name) {

    if (!name %in% names(cf$remotes))
        stop(sprintf("Provider entry '%s' not found.", name), call. = FALSE)

    href <- cf$remotes[[name]]$href

    con <- tryCatch(
        suppressWarnings(file(href)),
        error = function(e) {

            stop(sprintf(paste("Invalid file location '%s'.",
                               "Reported error: %s"), href, e$message), call. = FALSE)
        })

    res <- provider(con)

    close(con)

    return(res)
}

list_entries.eo_config_0.7 <- function(cf) {

    return(names(cf$remotes))
}
