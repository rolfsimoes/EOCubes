supported_versions <- function() {

    return(package_version(c("0.8", "0.7")))
}

new_object <- function(x, type, version) {

    version <- ifnull(version, supported_versions()[[1]])

    if (all(supported_versions() != version))
        stop("The version of config file is not supported.", call. = FALSE)

    x$version <- version
    res <- cast(structure(x, class = paste("eo", type, c(version, ""), sep = "_")))

    return(res)
}

cast <- function(...) {

    UseMethod("cast")
}

cast.eo_config_0.7 <- function(cf) {

    if (is.null(cf$remotes))
        stop("Invalid config file definition.", call. = FALSE)

    cf$remotes <- lapply(cf$remotes, new_object, type = "provider_link", version = cf$version)

    return(cf)
}

cast.eo_config_0.8 <- function(cf) {

    if ((cf$type != "#config") || is.null(cf$items))
        stop("Invalid config file definition.", call. = FALSE)

    return(cf)
}

cast.eo_provider_0.7 <- function(pr) {

    if (is.null(pr$description) || is.null(pr$cubes) || is.null(names(pr$cubes)) ||
        any(sapply(pr$cubes, function(x) is.null(x$href))))
        stop("Invalid provider file definition.", call. = FALSE)

    return(pr)
}

cast.eo_provider_0.8 <- function(pr) {

    if ((pr$type != "#provider") || is.null(pr$description) || is.null(pr$items) ||
        is.null(names(pr$items)) ||
        any(sapply(pr$items, function(x) (is.null(x$href) || is.null(x$type)))))
        stop("Invalid provider file definition.", call. = FALSE)

    return(pr)
}

cast.eo_cube_0.7 <- function(cb) {

    if (is.null(cb$id) || is.null(cb$meta) || is.null(cb$bands) || is.null(tiles))
        stop("Invalid cube file definition.", call. = FALSE)
}

cast.eo_cube_0.8 <- function(cb) {

    if (cb$type != "#cube")
        stop("Invalid cube file definition.", call. = FALSE)
}
