curator <- function(name, email) {

    res <- structure(list(name = name, email = email), class = "curator")

    return(res)
}

curator_list <- function(cur1, ...) {

    res <- c(list(cur1), list(...))

    if (!all(lapply(res, inherits, what = "curator")))
        stop("Invalid list of curators.", call. = FALSE)

    res <- structure(res, class = "curator_list")
    return(res)
}

new_provider <- function(type, version, description, keywords,
                         curators, items = list()) {

    if (any(c(!is.atomic(description), !is.atomic(keywords),
              !inherits(curators, "curator_list"))))
        stop("Invalid provider parameter values.", call. = FALSE)

    res <- new_catalog(type = type, version = version, description = description,
                       keywords = keywords, curators = curators, items = items)

    return(res)
}

default_provider <- function() {

    res <- new_provider(type = "provider", version = "0.8")
    res <- link_item(res, name = "localhost",
                     location = sprintf("%s/localhost/catalog.json", .local_base()))
    res <- link_item(res, name = "AWS.S3",
                     location = "https://eocubes-test.s3.amazonaws.com/catalog.json")

    return(res)
}

save_provider <- function(x, ...) {

    UseMethod("save_provider")
}

save_provider.EO_provider_0.8 <- function(x, file) {

    save_catalog(x = x, location = file)
    invisible(NULL)
}

load_provider <- function() {

    .global[["provider"]] <- load_catalog(location = sprintf("%s/provider.json", .local_base()),
                                        name = "provider")
    invisible(NULL)
}

link_provider <- function(name, location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a JSON file location.")

    .global[["provider"]] <- link_item(x = .global[["provider"]], name = name,
                                     location = location)

    save_provider()

    invisible(NULL)
}


unlink_provider <- function(name) {

    .global[["provider"]] <- unlink_item(x = .global[["provider"]], name = name)

    save_provider()

    invisible(NULL)
}


list_providers <- function(prefix = NULL) {

    res <- list_items(x = .global[["provider"]], prefix = prefix)

    return(res)
}

cast_catalog.EO_provider_0.8 <- function(x) {

    if (any(c(is.null(x$description), is.null(x$keywords),
              is.null(x$curators))))
        stop("Invalid provider data definition.", call. = FALSE)

    return(x)
}
