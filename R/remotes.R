#' @title Remotes management functions
#'
#' @name default_remote
#'
#' @description Fetches the default registered remote.
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

                message(sprintf(paste(
                    "Error when trying to save remotes list file '%s'.",
                    "Continuing anyway."), file))
            })
    } else
        stop(sprintf("Remote name '%s' not found", default))

    invisible(TRUE)
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
#' @export
#'
get_remote <- function(name) {

    if (!(name %in% names(.global[[.remotes]]$remotes)))
        stop(sprintf("Remote name '%s' not found.", name))
    res <- tryCatch(.open_json(.global[[.remotes]]$remotes[[name]]$href),
                    error = function(e) {
                        stop(sprintf("Remote file definition '%s' is unreachable.",
                                     .global[[.remotes]]$remotes[[name]]$href))
                    })
    return(res)
}

#' @title Remotes management functions
#'
#' @name list_remotes
#'
#' @description Lists all registered remotes.
#'
#' @return A \code{list} of remotes.
#'
#' @export
#'
list_remotes <- function() {

    res <- .global[[.remotes]]$remotes
    return(res)
}
