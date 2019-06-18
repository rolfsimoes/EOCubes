default_provider <- function() {

    res <- new_eo_object(list(version = "0.7",
                              description = "Local provider",
                              cubes = list()), type = "eo_provider")

    return(res)
}

provider <- function(...) {

    UseMethod("provider")
}

provider.character <- function(name) {

    res <- provider(.global[["conf"]], name = name)

    return(res)
}

provider.connection <- function(con) {

    res <- tryCatch(open_eo_object(con),
                    error = function(e) {

                        default <- default_provider()
                        save_json(default, con = con)
                        return(default)
                    })

    return(res)
}

save_provider <- function(pr, con) {

    save_eo_object(pr, con = con)

    invisible(NULL)
}

add_cube <- function(pr, cb) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    add_item(pr, cb, cube_name(cb))

    invisible(NULL)
}

del_cube <- function(pr, name) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    del_item(pr, name = name)

    invisible(NULL)
}

list_cubes <- function(pr) {

    if (!inherits(pr, "eo_provider"))
        stop("Invalid provider.", call. = FALSE)

    return(list_items(pr))
}

cube.eo_provider <- function(pr, name, bands = NULL, from = NULL, to = NULL,
                             geom = NULL, tiles = NULL, slices = NULL) {

    if (!exists_item(pr, name = name))
        stop(sprintf("Cube entry '%s' not found.", name), call. = FALSE)

    res <- open_entry(cast_entry(get_item(pr, name), default_type = "eo_cube")))

    res <- cube(res, bands = bands, from = from, to = to, geom = geom,
                tiles = tiles, slices = slices)

    return(res)
}
