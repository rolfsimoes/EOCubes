as_list <- function(...) {

    UseMethod("as_list")
}

as_list.eo_config <- function(cf) {

    class(cf) <- NULL

    return(cf)
}

as_list.eo_provider <- function(pr) {


}

as_list.eo_cube <- function(cb) {


}

list_providers <- function(...) {

    UseMethod("list_providers")
}

list_providers.eo_config <- function(cf) {


}

provider <- function(...) {

    UseMethod("provider")
}

provider.connection <- function(con) {


}

provider.character <- function(name) {


}

list_cubes <- function(...) {

    UseMethod("list_cubes")
}

list_cubes.eo_provider <- function(pr) {


}

cube <- function(...) {

    UseMethod("cube")
}

cube.eo_provider <- function(pr = provider("localhost"),
                             name, bands = NULL,
                             from = NULL, to = NULL,
                             geom = NULL, tiles = NULL) {


}

cube.eo_cube <- function(cb, bands = NULL,
                         from = NULL, to = NULL,
                         geom = NULL, tiles = NULL) {


}

cube.connection <- function(con, bands = NULL,
                            from = NULL, to = NULL,
                            geom = NULL, tiles = NULL) {


}

bbox <- function(...) {

    UseMethod("bbox")
}

bbox.eo_cube <- function(cb) {


}

bbox.numeric <- function(x1, y1 = NULL, x2 = NULL, y2 = NULL) {


}

interval <- function(...) {

    UseMethod("interval")
}

interval.eo_cube <- function(cb) {


}

crs <- function(...) {

    UseMethod("crs")
}

crs.eo_cube <- function(cb) {


}

bands <- function(...) {

    UseMethod("bands")
}

bands.eo_cube <- function(cb) {


}

tiles <- function(...) {

    UseMethod("tiles")
}

tiles.eo_cube <- function(cb) {


}

geom <- function(...) {


}

geom.eo_cube <- function(cb, tiles = NULL) {


}

info_bands <- function(...) {

    UseMethod("info_bands")
}

info_bands.eo_cube <- function(cb, bands = NULL) {


}

view <- function(...) {

    UseMethod("view")
}

view.eo_cube <- function(cb, st_date, st_period,
                         time_len) {


}

viewJSON <- function(...) {

    UseMethod("viewJSON")
}

viewJSON.eo_cube <- function(cb, st_date, st_period,
                             time_len) {


}

link <- function() {

    UseMethod("link")
}

link.eo_provider <- function(pr, name) {


}

link.eo_cube <- function(cb, pr, name) {


}

unlink <- function(...) {

    UseMethod("unlink")
}

unlink.eo_provider <- function(pr) {


}

unlink.eo_cube <- function(cb) {


}
