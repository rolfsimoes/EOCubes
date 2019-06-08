#' @title Internal functions
#'
#' @description Print the \code{EOCubes_repositorylist} object
#'
#' @param x   A \code{EOCubes_repositorylist} object to be printed.
#' @param ...   Any additional parameter.
#'
#' @return None.
#'
#' @export
#'
print.EOCubes_repositorylist <- function(x, ...) {

    values <- names(x)
    if (length(values) > 10)
        values <- values[1:10]
    for (i in seq_along(values)) {

        v <- values[[i]]
        cat(sprintf("[%s] \"%s\": %s", i, v, x[[v]]$description), "\n")
    }
    if (length(values) > 10)
        cat(sprintf("(%s line(s) ommited)", length(values) - 10), "\n")

    invisible(NULL)
}

#' @title Internal functions
#'
#' @description Print the \code{EOCubes_cubelist} object
#'
#' @param x   A \code{EOCubes_cubelist} object to be printed.
#' @param ...   Any additional parameter.
#'
#' @return None.
#'
#' @export
#'
print.EOCubes_cubelist <- function(x, ...) {

    values <- names(x)
    if (length(values) > 10)
        values <- values[1:10]
    for (i in seq_along(values)) {

        v <- values[[i]]
        cat(sprintf("[%s] \"%s\": %s", i, v, x[[v]]$description), "\n")
    }
    if (length(values) > 10)
        cat(sprintf("(%s line(s) ommited)", length(values) - 10), "\n")

    invisible(NULL)
}
