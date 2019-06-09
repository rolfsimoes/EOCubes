#' @title Provider functions
#'
#' @name provider_functions
#'
#' @description These functions provides a basic operations over providers.
#' A provider is an entry point a repository that can maintain many cubes.
#'
#' @param x   A \code{list} data structure to be converted
#' to \code{EO_provider} object.
#' @param prefix   A \code{character} containing provider/cube entry name
#' prefix to be filtered.
#' @param name   A \code{character} text with provider name.
#' @param caching   A \code{logical} value indicating wether to use cache system.
#' @param repos   An \code{EO_provider} object.
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

#' @describeIn provider_functions Define a single curator of a \code{provider_list}.
#'
#' @return A \code{curator} object.
#'
#' @details The function \code{curator} can be used with \code{curator_list} to generate
#' a list of curators of a provider.
#'
#' @export
#'
curator <- function(name, email) {

    res <- structure(list(name = name, email = email), class = "curator_provider")

    return(res)
}

#' @describeIn provider_functions Verify if a list is a set of \code{curator_provider}.
#'
#' @return A \code{logical} value.
#'
is_curator_list <- function(x) {

    res <- all(sapply(x, inherits, what = "curator_provider"))

    return(res)
}

#' @describeIn provider_functions Create a new \code{EO_provider} object.
#'
#' @return An \code{EO_provider} object.
#'
#' @export
#'
new_provider <- function(name, description, keywords, curators, items = list()) {

    if (any(c(!is.atomic(description), !is.atomic(keywords),
              !(inherits(curators, "curator_provider") || is_curator_list(curators)))))
        stop("Invalid provider parameter values.", call. = FALSE)

    if (inherits(curators, "curator_provider"))
        curators <- curator_list(cur1 = curators)

    res <- new_catalog(name = name, type = "provider", version = "0.8",
                       description = description, keywords = keywords,
                       curators = curators, items = items)

    return(res)
}

default_provider <- function() {

    res <- new_provider(name = "localhost", description = "Local provider",
                        keywords = c("Local", "EOCubes"),
                        curators = list(curator(name = "local user", email = "")))

    return(res)
}

#' @describeIn provider_functions Fetches a registered provider.
#'
#' @return An \code{EO_provider} object.
#'
#' @details The function \code{provider} fetches the provider registered on a given
#' entry \code{name}.
#'
#' @export
#'
provider <- function(name) {

    res <- open_item(x = config(), name = name)

    return(res)
}

#' @describeIn provider_functions Returns the name of a provider.
#'
#' @return A \code{character}.
#'
#' @details The function \code{provider_name} show the entry name of the provider
#' list from which the \code{EO_provider} object have been fetched.
#'
#' @export
#'
provider_name <- function(x, ...) {

    UseMethod("provider_name")
}

#' @method provider_name EO_provider
#'
#' @export
#'
provider_name.EO_provider <- function(x) {

    res <- catalog_name(x)

    return(res)
}

#' @describeIn provider_functions Save a provider definition in a file.
#'
#' @return None.
#'
#' @export
#'
save_provider <- function(x, ...) {

    UseMethod("save_provider")
}

#' @method save_provider EO_provider
#'
#' @export
#'
save_provider.EO_provider <- function(x, file = NULL) {

    save_catalog(x = x, file = file)
    invisible(NULL)
}

#' @describeIn provider_functions Load a provider definition from a location.
#'
#' @return An \code{EO_provider} object.
#'
#' @export
#'
load_provider <- function(...) {

    UseMethod("load_provider")
}

#' @method load_provider EO_provider
#'
#' @export
#'
load_provider.EO_provider <- function(location, name, cache = NULL) {

    res <- load_catalog(location = location, name = name, cache = cache)

    return(res)
}

#' @describeIn provider_functions Append a cube reference to a provider.
#'
#' @return An \code{EO_provider} object.
#'
#' @export
#'
link_cube <- function(x = provider("localhost"), ...) {

    UseMethod("link_cube")
}

#' @method link_cube EO_provider
#'
#' @export
#'
link_cube.EO_provider <- function(x, name, location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a JSON file location.")

    res <- link_item(x = x, name = name, location = location, require_type = "EO_cube")

    return(res)
}

#' @describeIn provider_functions Remove a cube reference from a provider.
#'
#' @return An \code{EO_provider} object.
#'
#' @export
#'
unlink_cube <- function(x, ...) {

    UseMethod("unlink_cube")
}

#' @method unlink_cube EO_provider
#'
#' @export
#'
unlink_cube.EO_provider <- function(x, name) {

    res <- unlink_item(x = x, name = name)

    return(res)
}

#' @describeIn provider_functions List all registered cubes in a provider.
#'
#' @return An \code{EO_provider_entries} object.
#'
#' @details The function \code{list_cubes} lists all registered cubes in a
#' provider. Use \code{prefix} parameter to filter the entries by name.
#'
#' @export
#'
list_cubes <- function(x = provider("localhost"), ...) {

    UseMethod("list_cubes")
}

#' @method list_cubes EO_provider
#'
#' @export
#'
list_cubes.EO_provider <- function(x, prefix = NULL) {

    res <- list_items(x = x, prefix = prefix)

    return(res)
}

as_entry.EO_provider <- function(x, location) {

    res <- list(description = x$description,
                href = location)

    return(res)
}

cast_catalog.EO_provider_0.8 <- function(x) {

    if (any(c(is.null(x$description), is.null(x$keywords),
              is.null(x$curators), is.null(x$items))))
        stop("Invalid provider data definition.", call. = FALSE)

    return(x)
}
