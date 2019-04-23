#' @title Remotes functions
#'
#' @name remote_functions
#'
#' @description These functions provides a basic operations over remotes.
#' A remote is an entry point to a cube provider that can maintain many cubes.
#'
#' @param prefix   A \code{character} containing remote/cube entry name
#' prefix to be filtered.
#' @param name   A \code{character} text with remote name.
#' @param cache   A \code{logical} value indicating wether to use cache system.
#' @param default   A \code{character} text with the new default remote name.
#' @param remote   An \code{EOCubes_remote} object.
#'
#' @seealso \code{\link{list_cubes}}
#'
#' @examples
#' list_remotes()
#' x <- remote("localhost")
#' remote_name(x)   # shows 'localhost'
#' list_cubes(x)   # list cubes in 'localhost'
#'
NULL

#' @describeIn remote_functions Lists all registered remotes.
#'
#' @return A \code{list} of remotes.
#'
#' @details The function \code{list_remotes} lists all registered remote
#' entries. Use \code{prefix} parameter to filter the entries by name.
#'
#' @export
#'
list_remotes <- function(prefix = NULL) {

    if (length(.global[["root"]]$remotes) == 0) {

        warning("No remote registered.", call. = FALSE)
        invisible(NULL)
    }

    if (is.null(prefix)) {

        res <- structure(.global[["root"]]$remotes, class = "EOCubes_remotelist")
        return(res)
    }

    selected <- .select_prefix(prefix, .global[["root"]]$remotes)

    if (!any(selected)) {

        warning(sprintf("No remote with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(.global[["root"]]$remotes[selected], class = "EOCubes_remotelist")
    return(res)
}

#' @describeIn remote_functions Fetches a registered remote.
#'
#' @return A \code{remote} data structure.
#'
#' @details The function \code{remote} fetches the remote registered on a given
#' entry \code{name}.
#'
#' @export
#'
remote <- function(name, cache = TRUE) {

    if (!(name %in% names(.global[["root"]]$remotes)))
        stop(sprintf("Remote name '%s' not found.", name), call. = FALSE)

    res <- .open_json(.global[["root"]]$remotes[[name]]$href, cache = cache)

    res <- structure(res, caching = cache, remote_name = name, class = "EOCubes_remote")
    return(res)
}

#' @describeIn remote_functions Fetches the default registered remote.
#'
#' @return A \code{EOCubes_remote} object.
#'
#' @details The function \code{default_remote} fetches the default remote in
#' the remote list. The default remote can be changed by providing a valid
#' remote name in \code{default} parameter.
#'
#' @export
#'
default_remote <- function(default = NULL, cache = TRUE) {

    if (missing(default))
        return(remote(.global[["root"]]$default, cache = cache))

    if (default %in% names(.global[["root"]]$remotes)) {

        .global[["root"]]$default <- default

        file <- sprintf("%s/root.json", .local_base)

        tryCatch(
            .save_json(.global[["root"]], file),

            error = function(e) {

                stop(sprintf(paste(
                    "Error when trying to save remotes list file '%s'.",
                    "Reported error: %s"), file, e$message), call. = FALSE)
            })

        return(remote(.global[["root"]]$default), cache = cache)
    } else
        stop(sprintf("Remote name '%s' not found", default), call. = FALSE)
}

#' @describeIn remote_functions Returns the name of a remote or a cube.
#'
#' @return A \code{character}.
#'
#' @details The function \code{remote_name} show the entry name of the remote
#' list from which the \code{EOCubes_remote} object have been fetched.
#'
#' @export
#'
remote_name <- function(remote) {

    if (!inherits(remote, "EOCubes_remote"))
        stop("You must inform an `EOCubes_remote` object as data input.", call. = FALSE)

    res <- attr(remote, "remote_name")
    return(res)
}

#' @describeIn remote_functions Lists all registered cubes in a remote.
#'
#' @return An \code{EOCubes_cubelist} object or \code{NULL} if no cube
#' satisfies the filter criteria.
#'
#' @details The function \code{list_cubes} lists all registered cubes entries
#' in a remote. Use \code{prefix} parameter to filter the entries by name.
#'
#' @export
#'
list_cubes <- function(remote = default_remote(), prefix = NULL) {

    if (!inherits(remote, "EOCubes_remote"))
        stop("You must inform an `EOCubes_remote` object as data input.", call. = FALSE)

    if (missing(remote))
        message(sprintf("Listing cubes of default remote: '%s'.", remote_name(remote)))

    if (length(remote$cubes) == 0) {

        warning("The remote has no cube.", call. = FALSE)
        invisible(NULL)
    }

    if (is.null(prefix)) {

        res <- structure(remote$cubes, class = "EOCubes_cubelist")
        return(res)
    }

    selected <- .select_prefix(prefix, remote$cubes)

    if (!any(selected)) {

        warning(sprintf("No cube with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(remote$cubes[selected], class = "EOCubes_cubelist")
    return(res)
}
