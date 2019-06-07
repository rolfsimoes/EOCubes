#' @title Repository functions
#'
#' @name repository_functions
#'
#' @description These functions provides a basic operations over repositories.
#' A repository is an entry point to a cube provider that can maintain many cubes.
#'
#' @param prefix   A \code{character} containing repository/cube entry name
#' prefix to be filtered.
#' @param x   A \code{list} data structure to be converted to repository object.
#' @param name   A \code{character} text with repository name.
#' @param cache   A \code{logical} value indicating wether to use cache system.
#' @param default   A \code{character} text with the new default repository name.
#' @param repository   An \code{EOCubes_repository} object.
#' @param location   An \code{character} string with a valid json location.
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

#' @describeIn repository_functions Convert a data structure to repository.
#'
#' @return A \code{repository} data structure.
#'
#' @export
#'
as_repository <- function(x, name, cache = FALSE) {

    if (any(c(is.null(x$version), is.null(x$description), is.null(x$keywords), is.null(x$cubes))))
        stop("Invalid repository data definition.", call. = FALSE)

    res <- structure(res, caching = cache, repository_name = name, class = c("EOCubes_repository"))

    res <-
        tryCatch(
            do.call(paste0(".as_repository_", x$version),
                    args = list(x = x, name = name, cache = cache)),
            error = function(e) stop("Repository definition version not supported.", call. = FALSE))
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
repository <- function(name, cache = TRUE) {

    if (!(name %in% names(.global[["root"]]$repositories)))
        stop(sprintf("Repository name '%s' not found.", name), call. = FALSE)

    res <- .open_json(.global[["root"]]$repositories[[name]]$href, cache = cache)

    res <- as_repository(res, name = name, cache = cache)
    return(res)
}

#' @describeIn repository_functions Fetches the default registered repository.
#'
#' @return A \code{EOCubes_repository} object.
#'
#' @details The function \code{default_repository} fetches the default repository in
#' the repository list. The default repository can be changed by providing a valid
#' repository name in \code{default} parameter.
#'
#' @export
#'
default_repository <- function(default = NULL, cache = TRUE) {

    if (missing(default))
        return(repository(.global[["root"]]$default, cache = cache))

    if (default %in% names(.global[["root"]]$repositories)) {

        .global[["root"]]$default <- default

        .save_root()

        return(repository(.global[["root"]]$default, cache = cache))
    } else
        stop(sprintf("Repository name '%s' not found", default), call. = FALSE)
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
repository_name <- function(repository) {

    if (!inherits(repository, "EOCubes_repository"))
        stop("You must inform an `EOCubes_repository` object as data input.", call. = FALSE)

    res <- attr(repository, "repository_name")
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
list_cubes <- function(repository = default_repository(), prefix = NULL) {

    if (!inherits(repository, "EOCubes_repository"))
        stop("You must inform an `EOCubes_repository` object as data input.", call. = FALSE)

    if (missing(repository))
        message(sprintf("Listing cubes of default repository: '%s'.", repository_name(repository)))

    if (length(repository$cubes) == 0) {

        warning("The repository has no cube.", call. = FALSE)
        invisible(NULL)
    }

    if (is.null(prefix)) {

        res <- structure(repository$cubes, class = "EOCubes_cubelist")
        return(res)
    }

    selected <- .select_prefix(prefix, repository$cubes)

    if (!any(selected)) {

        warning(sprintf("No cube with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(repository$cubes[selected], class = "EOCubes_cubelist")
    return(res)
}

#' @describeIn repository_functions Adds a repository definition to the package root.
#'
#' @return A \code{repository} data structure.
#'
#' @details The function \code{add_repository} fetches the repository definition and add an
#' entry into the package root using \code{name} as key.
#'
#' @export
#'
add_repository <- function(name, location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a 'json' file location.")

    res <- .open_json(location, cache = FALSE)

    res <- as_repository(res, name = name, cache = FALSE)

    if (name %in% names(.global[["root"]]$repositories))
        stop(sprintf("Repository '%s' already exists.", name), call. = FALSE)

    .global[["root"]]$repositories[[name]] <- res["description", "keywords", href = location]

    .save_root()

    return(res)
}

#' @describeIn repository_functions Remove a repository entry from the package root.
#'
#' @return None.
#'
#' @details The function \code{rm_repository} remove the repository entry from the package root.
#'
#' @export
#'
rm_repository <- function(name) {

    if (!(name %in% names(.global[["root"]]$repositories)))
        stop(sprintf("Repository '%s' does not exist.", name), call. = FALSE)

    file <- .global[["root"]]$repositories[[name]]$href
    message(sprintf("The repository reference '%s' was removed.", file))

    .global[["root"]]$repositories[[name]] <- NULL

    .save_root()

    return(res)
}

#' @describeIn repository_functions Add a cube reference to the local repository.
#'
#' @return A \code{cube} data structure.
#'
#' @details The function \code{add_cube} fetches the repository definition and add an
#' entry into the package root using \code{name} as key.
#'
#' @export
#'
add_cube <- function(name, location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a 'json' file location.")

    res <- .open_json(location, cache = FALSE)

    res <- as_repository(res, name = name, cache = FALSE)

    if (name %in% names(.global[["root"]]$repositories))
        stop(sprintf("Repository '%s' already exists.", name), call. = FALSE)

    .global[["root"]]$repositories[[name]] <- res["description", "keywords", href = location]

    .save_root()

    return(res)
}

#' @describeIn repository_functions Remove a cube reference.
#'
#' @return None.
#'
#' @details The function \code{rm_cube} removes the cube reference from the repository 'localhost'.
#'
#' @export
#'
rm_cube <- function(name) {

    if (!(name %in% names(.global[["root"]]$repositories)))
        stop(sprintf("Repository '%s' does not exist.", name), call. = FALSE)

    file <- .global[["root"]]$repositories[[name]]$href
    message(sprintf("The remore reference to '%s' was removed.", file))

    .global[["root"]]$repositories[[name]] <- NULL

    .save_root()

    return(res)
}

.as_repository_0.7 <- function(repository) {

    return(repository)
}
