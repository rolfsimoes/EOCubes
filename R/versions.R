#' @title Version control functions
#'
#' @name version_functions
#'
#' @param x   A \code{list} data structure to be converted to repository object.
#' @param name   A \code{character} text with repository name.
#' @param repository   An \code{EOCubes_repository} object.
#' @param cube   An \code{EOCubes_cube} object.
#'
#' @description These functions provides the basic operations over a cube
#' (\code{EOCubes_cube}) object.
#'
#' @seealso \code{\link{repository}}
#'
NULL

is_supported <- function(x) {

    UseMethod("is_supported", object = x)
}

is_supported.EOCubes_repository_0.7 <- function(repository) {

    return(TRUE)
}

is_supported.EOCubes_cube_0.7 <- function(cube) {

    return(TRUE)
}
