as_list <- function(...) {

    UseMethod("as_list")
}

as_list.default <- function(x) {

    class(x) <- NULL

    return(x)
}

cast <- function(...) {

    UseMethod("cast")
}

list_providers <- function(...) {

    UseMethod("list_providers")
}

provider <- function(...) {

    UseMethod("provider")
}

provider.connection <- function(con) {

    res <- new_object(open_json(con), "eo_provider")

    attr(res, "reference") <- summary(con)[["description"]]

    return(res)
}

provider.character <- function(name) {

    res <- provider(.global[["conf"]], name = name)

    return(res)
}

link_provider <- function(pr, name) {

    link(.global[["conf"]], pr = pr, name = name)

    invisible(NULL)
}

link_cube <- function(pr, cb) {

    if (is.null(cb$id))
        stop("Invalid cube data definition.", call. = FALSE)

    link(pr, reference(cb), cb$id)
}

list_cubes <- function(...) {

    UseMethod("list_cubes")
}

cube <- function(...) {

    UseMethod("cube")
}

cube.connection <- function(con, select_bands = NULL,
                            interval_from = NULL, interval_to = NULL,
                            in_geometry = NULL, select_tiles = NULL) {

    res <- new_object(open_json(con), "eo_cube")

    res <- cube(res, select_bands = select_bands,
                interval_from = interval_from, interval_to = interval_to,
                in_geometry = in_geometry, select_tiles = select_tiles)

    attr(res, "reference") <- summary(con)[["description"]]

    return(res)
}

bbox <- function(...) {

    UseMethod("bbox")
}

bbox.numeric <- function(x) {

    if (length(x1) != 4)
        stop("Invalid bbox parameters.", call. = FALSE)

    res <- c(xmin = x[[1]], ymin = x[[2]], xmax = x[[3]], ymax = x[[4]])
    class(res) <- "bbox"

    return(res)
}

interval <- function(...) {

    UseMethod("interval")
}

interval.default <- function(from, to) {

    res <- list(from = from, to = to)
    class(res) <- "interval"

    return(res)
}

crs <- function(...) {

    UseMethod("crs")
}

bands <- function(...) {

    UseMethod("bands")
}

tiles <- function(...) {

    UseMethod("tiles")
}

geom <- function(...) {


}

info_bands <- function(...) {

    UseMethod("info_bands")
}

view <- function(...) {

    UseMethod("view")
}

viewJSON <- function(cb, st_date, st_period, time_len) {


}

description <- function(...) {

    UseMethod("description")
}

