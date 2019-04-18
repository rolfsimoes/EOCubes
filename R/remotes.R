#' @title Remotes management functions
#'
#' @name default_remote
#'
#' @description Fetches the default registered remote.
#'
#' @param default   A \code{character} text with the new default remote name.
#'
#' @return A \code{character} text with the name of default remote.
#'
#' @export
#'
default_remote <- function(default = NULL) {

    if (missing(default))
        return(.global[[.remotes]]$default)

    if (default %in% names(.global[[.remotes]]$remotes)) {

        .global[[.remotes]]$default <- default

        file <- sprintf("%s/remotes.json", .local_base)

        tryCatch(
            .save_json(.global[[.remotes]], file),

            error = function(e) {

                stop(sprintf(paste(
                    "Error when trying to save remotes list file '%s'.",
                    "Reported error: %s"), file, e$message))
            })

        return(.global[[.remotes]]$default)
    } else
        stop(sprintf("Remote name '%s' not found", default))
}

#' @title Remotes management functions
#'
#' @name get_remote
#'
#' @description Fetches a registered remote.
#'
#' @param name   A \code{character} text with remote name.
#'
#' @return A \code{remote} data structure.
#'
.fetch_remote <- function(name) {

    if (!(name %in% names(.global[[.remotes]]$remotes)))
        stop(sprintf("Remote name '%s' not found.", name))

    res <- .open_json(.global[[.remotes]]$remotes[[name]]$href)

    res <- structure(res, class = "EOCubes_remote")
    return(res)
}

#' @title Remotes management functions
#'
#' @name list_remotes
#'
#' @description Lists all registered remotes.
#'
#' @param prefix   A \code{character} containing remote name prefix to be filtered.
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
