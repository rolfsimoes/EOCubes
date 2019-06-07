#' @title Management functions
#'
#' @name management_functions
#'
#' @description These functions provides a basic management of repositories.
#' A repository is an entry point to a cube provider that can maintain many cubes.
#'
#' @param name   A \code{character} text with repository or cube entry name.
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


#' @title Internal functions
#'
#' @name .save_root
#'
#' @description Save the current root state.
#'
#' @return None.
#'
.save_root <- function() {

    base <- path.expand(.local_base)

    file <- sprintf("%s/root.json", base)

    tryCatch(
        .save_json(.global[["root"]], file),

        error = function(e) {

            stop(sprintf(paste(
                "Error when trying to save package root '%s'.",
                "Reported error: %s"), file, e$message), call. = FALSE)
        })

    invisible(NULL)
}

#' @title Internal functions
#'
#' @name .load_root
#'
#' @description Load the current root state.
#'
#' @return None.
#'
.load_root <- function() {

    base <- path.expand(.local_base)

    file <- sprintf("%s/root.json", base)

    repositories <- tryCatch(
        .open_json(file, cache = FALSE),
        error = function(e) {

            message(sprintf(paste(
                "Error when trying to load package root '%s'.",
                "Loading default repository list."), file))
            return(.default_repositories)
        })

    .global[["root"]] <- repositories

    invisible(NULL)
}

#' @describeIn management_functions Adds a repository definition to the package root.
#'
#' @return A \code{repository} object.
#'
#' @details The function \code{link_repository} fetches the repository definition and add an
#' entry into the package root using \code{name} as key.
#'
#' @export
#'
link_repository <- function(name, location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a 'json' file location.")

    if (name %in% names(.global[["root"]]$repositories))
        stop(sprintf("The repository '%s' already exists.", name), call. = FALSE)

    res <- .open_json(location, cache = FALSE)

    res <- as_repository(res, name = name, caching = FALSE)

    .global[["root"]]$repositories[[name]] <- c(res[c("description", "keywords")], href = location)

    .save_root()

    return(res)
}

#' @describeIn management_functions Remove a repository entry from the package root.
#'
#' @return None.
#'
#' @details The function \code{rm_repository} remove the repository entry from the package root.
#'
#' @export
#'
unlink_repository <- function(name) {

    if (!(name %in% names(.global[["root"]]$repositories)))
        stop(sprintf("The repository '%s' does not exist.", name), call. = FALSE)

    file <- .global[["root"]]$repositories[[name]]$href
    message(sprintf("The repository in '%s' was unlinked.", file))

    .global[["root"]]$repositories[[name]] <- NULL

    .save_root()

    invisible(NULL)
}

#' @describeIn management_functions Add a cube reference to the local repository.
#'
#' @return A \code{cube} object.
#'
#' @details The function \code{add_cube} fetches the repository definition and add an
#' entry into the package root using \code{name} as key.
#'
#' @export
#'
link_cube <- function(location) {

    if (!grepl("^.+\\.json$", location))
        stop("Inform a 'json' file location.")

    res <- .open_json(location, cache = FALSE)

    res <- as_cube(res, name = "localhost", caching = FALSE)

    name <- cube_name(res)

    if (name %in% names(.global[["root"]]$repositories[["localhost"]]$cubes))
        stop(sprintf("Repository '%s' already exists in local repository.", name), call. = FALSE)

    .global[["root"]]$repositories[["localhost"]]$cubes[[name]] <- c(res[c("description", "keywords")], href = location)

    .save_root()

    return(res)
}

#' @describeIn management_functions Remove a cube reference.
#'
#' @return None.
#'
#' @details The function \code{rm_cube} removes the cube reference from the repository 'localhost'.
#'
#' @export
#'
unlink_cube <- function(name) {

    if (!(name %in% names(.global[["root"]]$repositories[["localhost"]]$cubes)))
        stop(sprintf("Repository '%s' does not exist in local repository.", name), call. = FALSE)

    file <- .global[["root"]]$repositories[["localhost"]]$cubes[[name]]$href
    message(sprintf("The cube located in '%s' was removed from local repository.", file))

    .global[["root"]]$repositories[["localhost"]]$cubes[[name]] <- NULL

    .save_root()

    invisible(NULL)
}
