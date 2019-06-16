
cube <- function(...) {

    UseMethod("cube")
}

cube.connection <- function(con, bands = NULL, from = NULL, to = NULL, geom = NULL, tiles = NULL) {

    res <- cast(open_json(con), default_type = "eo_cube")

    res <- cube(res, bands = bands, from = from, to = to, geom = geom, tiles = tiles)

    return(res)
}

viewJSON <- function(cb, st_date, st_period, time_len) {


}

#### cube get/set ####

cube_name <- function(...) {

    UseMethod("cube_name")
}

cube_bands <- function(...) {

    UseMethod("cube_bands")
}

`cube_bands<-` <- function(...) {

    UseMethod("cube_bands<-")
}

interval <- function(...) {

    UseMethod("interval")
}

`interval<-` <- function(...) {

    UseMethod("interval<-")
}

cube_geom <- function(...) {

    UseMethod("cube_geom")
}

`cube_geom<-` <- function(...) {

    UseMethod("cube_geom<-")
}

cube_tiles <- function(...) {

    UseMethod("cube_tiles")
}

`cube_tiles<-` <- function(...) {

    UseMethod("cube_tiles<-")
}

cube_crs <- function(...) {

    UseMethod("cube_crs")
}

bbox <- function(...) {

    UseMethod("bbox")
}

info_bands <- function(...) {

    UseMethod("info_bands")
}

view <- function(...) {

    UseMethod("view")
}
