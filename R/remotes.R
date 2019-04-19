#' @title Remotes functions
#'
#' @name remote_functions
#'
#' @description These functions provides a basic operations over remotes.
#' A remote is an entry point to a cube provider that can maintain many cubes.
#'
#' @param prefix   A \code{character} containing remote name prefix to be
#' filtered.
#' @param name   A \code{character} text with remote name.
#' @param default   A \code{character} text with the new default remote name.
#' @param remote   An \code{EOCubes_remote} object.
#'
#' @seealso \code{\link{list_cubes}}
#' @examples
#' list_remotes()
#' x <- remote("localhost")
#' remote_name(x)  # shows 'localhost'
#'
NULL

#' @describeIn remote_functions Lists all registered remotes.
#'
#' @return A \code{list} of remotes.
#'
#' @export
#'
list_remotes <- function(prefix = NULL) {

    res <- .filter_prefix(prefix, .global[[.remotes]]$remotes)

    res <- structure(res, class = "EOCubes_remotelist")
    return(res)
}

#' @describeIn remote_functions Fetches a registered remote.
#'
#' @return A \code{remote} data structure.
#'
#' @export
#'
remote <- function(name) {

    if (!(name %in% names(.global[[.remotes]]$remotes)))
        stop(sprintf("Remote name '%s' not found.", name), call. = FALSE)

    res <- .open_json(.global[[.remotes]]$remotes[[name]]$href)

    res <- structure(res, remote_name = name, class = "EOCubes_remote")
    return(res)
}

#' @describeIn remote_functions Fetches the default registered remote.
#'
#' @return A \code{character} text with the name of default remote.
#'
#' @export
#'
default_remote <- function(default = NULL) {

    if (missing(default))
        return(remote(.global[[.remotes]]$default))

    if (default %in% names(.global[[.remotes]]$remotes)) {

        .global[[.remotes]]$default <- default

        file <- sprintf("%s/remotes.json", .local_base)

        tryCatch(
            .save_json(.global[[.remotes]], file),

            error = function(e) {

                stop(sprintf(paste(
                    "Error when trying to save remotes list file '%s'.",
                    "Reported error: %s"), file, e$message), call. = FALSE)
            })

        return(remote(.global[[.remotes]]$default))
    } else
        stop(sprintf("Remote name '%s' not found", default), call. = FALSE)
}

#' @describeIn remote_functions Returns the name of a remote or a cube.
#'
#' @return A \code{character}.
#'
#' @export
#'
remote_name <- function(remote) {

    if (!any(c("EOCubes_remote", "EOCubes_cube") %in% class(remote)))
        stop("You must inform an `EOCubes_remote` object as data input.", call. = FALSE)

    return(attr(remote, "remote_name"))
}

#' @title Remote functions
#'
#' @name list_cubes
#'
#' @description Lists all registered cubes in a remote.
#'
#' @param remote   An \code{EOCubes_remote} object.
#' @param prefix   A \code{character} containing cube name prefix to be
#' filtered.
#'
#' @return An \code{EOCubes_cubelist} object or \code{NULL} if no cube
#' satisfies the filter criteria.
#'
#' @examples
#' list_cubes()  # list cubes from default remote
#'
#' @export
#'
list_cubes <- function(remote = default_remote(), prefix = NULL) {

    if (!("EOCubes_remote" %in% class(remote)))
        stop("You must inform an `EOCubes_remote` object as data input.", call. = FALSE)

    if (missing(remote))
        message(sprintf("Listing cubes of default remote: '%s'.", remote_name(remote)))

    res <- .filter_prefix(prefix, remote$cubes)

    if (length(res) == 0) {

        warning(sprintf("No cube with prefix '%s' was found.", prefix), call. = FALSE)
        invisible(NULL)
    }

    res <- structure(res, class = "EOCubes_cubelist")
    return(res)
}
