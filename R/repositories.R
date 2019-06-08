#' @title Repository functions
#'
#' @name repository_functions
#'
#' @description These functions provides a basic operations over repositories.
#' A repository is an entry point to a cube provider that can maintain many cubes.
#'
#' @param x   A \code{list} data structure to be converted
#' to \code{EOCubes_repository} object.
#' @param prefix   A \code{character} containing repository/cube entry name
#' prefix to be filtered.
#' @param name   A \code{character} text with repository name.
#' @param caching   A \code{logical} value indicating wether to use cache system.
#' @param repos   An \code{EOCubes_repository} object.
#'
#' @seealso \code{\link{list_cubes}}
#'
#' @examples
#' list_repositories()
#' x <- repository("localhost")
#' repository_name(x)   # shows 'localhost'
#' list_cubes(x)   # list cubes in 'localhost'
#'
NULL

#' @describeIn repository_functions Return the default localhost repository.
#'
#' @return A \code{EOCubes_repository} object.
#'
.default_localhost <- function() {

    res <- structure(list(
        id = "localhost",
        version = "0.7",
        description = "Local repository",
        keywords = list(c("EOCubes", "Local", "Repository")),
        cubes = list()), class = "EOCubes_repository")

    return(res)
}

#' @describeIn repository_functions Convert a well-formed data structure to
#' a repository object.
#'
#' @return A \code{repository} object.
#'
#' @export
#'
as_repository <- function(x, name, caching) {

    if (any(c(is.null(x$version), is.null(x$description), is.null(x$keywords), is.null(x$cubes))))
        stop("Invalid repository data definition.", call. = FALSE)

    res <- structure(x,
                     repository_name = name,
                     caching = caching,
                     class = c(paste0("EOCubes_repository_", x$version), "EOCubes_repository"))

    tryCatch(is_supported(res),
             error = function(e) stop("The version of the respository definition is not supported.", call. = FALSE))

    return(res)
}

#' @describeIn repository_functions Lists all registered repositories.
#'
#' @return A \code{list} of repositories.
#'
#' @details The function \code{list_repositories} lists all registered repository
#' entries. Use \code{prefix} parameter to filter the entries by name.
#'
#' @export
#'
list_repositories <- function(prefix = NULL) {

    if (length(.global[["root"]]$repositories) == 0) {

        warning("No repository registered.", call. = FALSE)
        invisible(NULL)
    }

    if (is.null(prefix)) {

        res <- structure(.global[["root"]]$repositories, class = "EOCubes_repositorylist")
        return(res)
    }

    selected <- .select_prefix(prefix, .global[["root"]]$repositories)

    if (!any(selected)) {

        warning(sprintf("No repository with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(.global[["root"]]$repositories[selected], class = "EOCubes_repositorylist")
    return(res)
}

#' @describeIn repository_functions Fetches a registered repository.
#'
#' @return A \code{repository} data structure.
#'
#' @details The function \code{repository} fetches the repository registered on a given
#' entry \code{name}.
#'
#' @export
#'
repository <- function(name, caching = TRUE) {

    if (!(name %in% names(.global[["root"]]$repositories)))
        stop(sprintf("Repository name '%s' not found.", name), call. = FALSE)

    res <- .open_json(.global[["root"]]$repositories[[name]]$href, cache = caching)

    res <- as_repository(res, name = name, caching = caching)

    return(res)
}

#' @describeIn repository_functions Returns the name of a repository or a cube.
#'
#' @return A \code{character}.
#'
#' @details The function \code{repository_name} show the entry name of the repository
#' list from which the \code{EOCubes_repository} object have been fetched.
#'
#' @export
#'
repository_name <- function(repos) {

    if (!inherits(repos, "EOCubes_repository"))
        stop("You must inform an `EOCubes_repository` object as data input.", call. = FALSE)

    res <- attr(repos, "repository_name")
    return(res)
}

#' @describeIn repository_functions Lists all registered cubes in a repository.
#'
#' @return An \code{EOCubes_cubelist} object or \code{NULL} if no cube
#' satisfies the filter criteria.
#'
#' @details The function \code{list_cubes} lists all registered cubes entries
#' in a repository. Use \code{prefix} parameter to filter the entries by name.
#'
#' @export
#'
list_cubes <- function(repos = repository("localhost"), prefix = NULL) {

    if (!inherits(repos, "EOCubes_repository"))
        stop("You must inform an `EOCubes_repository` object as data input.", call. = FALSE)

    if (missing(repos))
        message(sprintf("Listing cubes of default repository: '%s'.", repository_name(repos)))

    if (length(repos$cubes) == 0) {

        warning("The repository has no cube.", call. = FALSE)
        invisible(NULL)
    }

    if (is.null(prefix)) {

        res <- structure(repos$cubes, class = "EOCubes_cubelist")
        return(res)
    }

    selected <- .select_prefix(prefix, repos$cubes)

    if (!any(selected)) {

        warning(sprintf("No cube with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(repos$cubes[selected], class = "EOCubes_cubelist")
    return(res)
}
