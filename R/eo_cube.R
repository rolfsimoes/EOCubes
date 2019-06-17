cube <- function(...) {

    UseMethod("cube")
}

cube.eo_cube <- function(cb, bands = NULL, from = NULL, to = NULL, geom = NULL,
                         tiles = NULL, slices = NULL) {

    if (!missing(bands))
        cube_bands(cb) <- bands

    if (!missing(from) && !missing(to))
        cube_interval(cb) <- interval(from, to)

    if (!missing(geom))
        cube_geom(cb) <- geom

    if (!missing(tiles))
        cube_tiles(cb) <- tiles

    if (!missing(slices))
        cube_slices(cb) <- slices

    return(cb)
}

cube.connection <- function(con, bands = NULL, from = NULL, to = NULL,
                            geom = NULL, tiles = NULL, slices = NULL) {

    res <- cast(open_json(con), default_type = "eo_cube")

    res <- cube(res, bands = bands, from = from, to = to, geom = geom,
                tiles = tiles, slice = slices)

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

cube_interval <- function(...) {

    UseMethod("cube_interval")
}

`cube_interval<-` <- function(...) {

    UseMethod("cube_interval<-")
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

cube_slices <- function(...) {

    UseMethod("cube_slices")
}

`cube_slices<-` <- function(...) {

    UseMethod("cube_slices<-")
}

cube_crs <- function(...) {

    UseMethod("cube_crs")
}

cube_bbox <- function(...) {

    UseMethod("cube_bbox")
}

info_bands <- function(...) {

    UseMethod("info_bands")
}

fetch <- function(...) {

    UseMethod("fetch")
}

#### tiles get/set ####

tiles_bbox <- function(...) {

    UseMethod("tiles_bbox")
}

tiles_geom <- function(...) {

    UseMethod("tiles_geom")
}
