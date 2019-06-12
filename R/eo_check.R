supported_versions <- function() {

    return(package_version(c("0.8", "0.7")))
}

class_name <- function(type, version) {

    version <- ifnull(version, supported_versions()[[1]])

    if (all(supported_versions() != version))
        stop("The version of config file is not supported.", call. = FALSE)

    return(paste("eo", type, c(version, ""), sep = "_"))
}

check <- function(...) {

    UseMethod("check")
}

check.eo_config_0.7 <- function(cf) {

    if (is.null(cf$remotes))
        stop("Invalid config file definition.", call. = FALSE)

    return(cf)
}

check.eo_config_0.8 <- function(cf) {

    if ((cf$type != "#config") || is.null(cf$items))
        stop("Invalid config file definition.", call. = FALSE)

    return(cf)
}

check.eo_provider_0.7 <- function(pr) {

    if (is.null(pr$description) || is.null(pr$cubes) || is.null(names(pr$cubes)) ||
        any(sapply(pr$cubes, function(x) is.null(x$href))))
        stop("Invalid provider file definition.", call. = FALSE)

    return(pr)
}

check.eo_provider_0.8 <- function(pr) {

    if ((pr$type != "#provider") || is.null(pr$description) || is.null(pr$items) ||
        is.null(names(pr$items)) ||
        any(sapply(pr$items, function(x) (is.null(x$href) || is.null(x$type)))))
        stop("Invalid provider file definition.", call. = FALSE)

    return(pr)
}

check.eo_cube_0.7 <- function(cb) {

    if (is.null(cb$id) || is.null(cb$meta) || is.null(cb$bands) || is.null(tiles))
        stop("Invalid cube file definition.", call. = FALSE)
}

check.eo_cube_0.8 <- function(cb) {

    if (cb$type != "#cube")
        stop("Invalid cube file definition.", call. = FALSE)
}
