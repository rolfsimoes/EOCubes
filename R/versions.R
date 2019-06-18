supported_versions <- function(type = NULL) {

    res <- list(eo_config = package_version("0.7"),
                eo_provider = package_version("0.7"),
                eo_cube = package_version("0.7"))

    if (is.null(type))
        return(res)

    if (!type %in% names(res))
        stop("The catalog `type` is not supported.", call. = FALSE)

    return(res[[type]])
}
