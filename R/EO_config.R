#' @title Config functions
#'
#' @name config_functions
#'
#' @description These functions provides a basic management of providers.
#' A provider is an entry point to a cube provider that can maintain many
#' cubes.
#'
#' @param name   A \code{character} text with provider entry name.
#' @param location   An \code{character} string with a valid json location.
#' @param prefix   A \code{character} containing an prefix entry name
#' to be filtered.
#'
#' @seealso \code{\link{list_cubes}}
#'
#' @examples
#' list_providers()
#' x <- provider("localhost")
#' provider_name(x)   # shows 'localhost'
#' list_cubes(x)   # list cubes in 'localhost'
#'
NULL

new_config <- function(type, version, items = list()) {

    res <- new_catalog(type = type, version = version, items = items)

    return(res)
}

default_config <- function() {

    res <- new_config(type = "config", version = "0.8")
    res <- link_item(res, name = "localhost",
                     location = sprintf("%s/localhost/catalog.json", .local_base()))
    res <- link_item(res, name = "AWS.S3",
                     location = "https://eocubes-test.s3.amazonaws.com/catalog.json")

    return(res)
}

#' @describeIn config_functions Save the current config state.
#'
#' @return None.
#'
#' @export
#'
save_config <- function() {

    save_catalog(x = .global[["config"]],
                 location = sprintf("%s/config.json", .local_base()))
    invisible(NULL)
}

#' @describeIn config_functions Load the current config state.
#'
#' @return None.
#'
#' @export
#'
load_config <- function() {

    .global[["config"]] <- load_catalog(location = sprintf("%s/config.json", .local_base()),
                                        name = "config")
    invisible(NULL)
}

#' @describeIn config_functions Adds a provider definition to the package
#' config.
#'
#' @return A \code{EO_provider} object.
#'
#' @details The function \code{link_provider} fetches the provider
#' definition and add an entry into the package config using \code{name} as key.
#'
#' @export
#'
link_provider <- function(name, location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a JSON file location.")

    .global[["config"]] <- link_item(x = .global[["config"]], name = name,
                                     location = location)

    save_config()

    invisible(NULL)
}

#' @describeIn config_functions Remove a provider entry from the package
#' config.
#'
#' @return None.
#'
#' @export
#'
unlink_provider <- function(name) {

    .global[["config"]] <- unlink_item(x = .global[["config"]], name = name)

    save_config()

    invisible(NULL)
}

#' @describeIn config_functions Lists all registered providers.
#'
#' @return A \code{list} of providers entries.
#'
#' @details The function \code{list_providers} lists all registered
#' providers entries. Use \code{prefix} parameter to filter the entries by
#' name.
#'
#' @export
#'
list_providers <- function(prefix = NULL) {

    res <- list_items(x = .global[["config"]], prefix = prefix)

    return(res)
}

cast_catalog.EO_config_0.8 <- function(x) {

    return(x)
}
