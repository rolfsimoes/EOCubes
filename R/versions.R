supported_versions <- function() {

    return(package_version(c("0.8", "0.7")))
}

new_object <- function(x, type) {

    version <- ifnull(x$version, supported_versions()[[1]])

    if (all(supported_versions() != version))
        stop("The version of config file is not supported.", call. = FALSE)

    x$version <- version
    res <- structure(x, class = paste(type, c(version, ""), sep = "_"))

    return(res)
}
