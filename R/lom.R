
# list object model definitions
.template.manifest <- list(
    "description" = "!description",
    "authors"     = "@authors",
    "references"  = "@references",
    "citation"    = "!citation",
    "data_policy" = "!data_policy",
    "maintainer"  = "!maintainer",
    "definition"  = "!definition"
)

# list object model definitions
.template.bands <- list(
    ":band_long_name" = list(
        "band_short_name" = "!band_short_name",
        "min"             = "!min",
        "max"             = "!max",
        "fill"            = "!fill",
        "scale"           = "!scale"
    )
)

# list object model definitions
.template.bricks <- list(
    ":key" = list(
        "nrow"     = "!nrow",
        "ncol"     = "!ncol",
        "time_len" = "!time_len",
        "crs"      = "!crs",
        "xmin"     = "!xmin",
        "xmax"     = "!xmax",
        "ymin"     = "!ymin",
        "ymax"     = "!ymax",
        "xres"     = "!xres",
        "yres"     = "!yres",
        "rasters"  = list(
            ":band_long_name" = list(
                "file"      = "!file",
                "data_type" = "!data_type"
            )
        )
    )
)

.template.timeline <- list(
    "timeline" = "!timeline"
)

.as_lom <- function(df, template) {

    .get_v <- function(x, df, ...) {

        result <- lapply(x, function(n) {
            m <- substr(n, 1, 1)
            s <- substr(n, 2, nchar(n))
            if (m == "!") {
                if (is.null(df[[s]])) {

                    stop(sprintf("The value field '%s' is missing.", s))
                }
                result <- unique(df[[s]])
                if (length(result) > 1)
                    warning(sprintf(
                        "Non unique values %s in unique (!) field '%s' grouped by %s.",
                        .sublime_list(result), s,
                        .sublime_list(list(...))),
                        call. = FALSE)
                return(result)
            } else if (m == "@") {
                if (is.null(df[[s]])) {

                    stop(sprintf("The value field '%s' is missing.", s))
                }
                return(unique(df[[s]]))
            }
            if (is.null(df[[n]])) {

                stop(sprintf("The value field '%s' is missing.", n))
            }
            return(df[[n]])
        })

        unlist(result, recursive = FALSE)
    }

    .grp_by <- function(t, df, f, ...) {
        result <- lapply(t, function(x) {
            c(by(df, f, function(.df) {
                if (is.list(x)) {
                    list(.proc(x, .df, ...))
                } else {
                    .get_v(x, .df, ...)
                }
            }))
        })
        names(result) <- NULL
        unlist(result, recursive = F)
    }

    .nest <- function(t, df, ...) {
        lapply(t, function(x) {
            if (is.list(x)) {
                .proc(x, df, ...)
            } else {
                .get_v(x, df, ...)
            }
        })
    }

    .proc <- function(t, df, ...) {
        n <- names(t)
        result <- lapply(seq_along(t), function(i) {

            if (is.null(n)) return(.nest(t[i], df, ...))
            m <- substr(n[i], 1, 1)
            s <- substr(n[i], 2, nchar(n[i]))
            if (m == ":") {
                if (is.null(df[[s]])) {

                    stop(sprintf("The group field '%s' is missing.", s))
                }
                result <- .grp_by(t[i], df, df[[s]], ..., s)
                names(result) = paste0(s, ":", names(result))
                result
            } else if (m == "-") {
                result <- .grp_by(t[i], df, 1:nrow(df), ...)
                names(result) <- NULL
                result <- list(result)
                names(result) <- s
                result
            } else {
                .nest(t[i], df, ...)
            }
        })
        unlist(result, recursive = F)
    }

    .proc(template, df)
}

.as_df <- function(mo) {

    .grouped_mo <- function(mo, n) {
        names(mo) <- NULL

        res <- mapply(function(x, y) {
            f <- gsub("(.+):.+", "\\1", y)
            v <- gsub(".+:(.+)", "\\1", y)
            g <- list(v)
            names(g) <- f
            if (!is.list(x)) {
                res <- list(value = x)
            } else if (is.null(names(x))) {
                res <- .listed_mo(x, y)
            } else if (any(grepl(".+:.+", names(x)))) {
                res <- .grouped_mo(x, names(x))
            } else {
                res <- .nested_mo(x, names(x))
            }
            res <- append(g, res)
            res
        }, mo, n, SIMPLIFY = FALSE)

        res <- Reduce(function(x, y) {
            mapply(c, x, y, SIMPLIFY = FALSE)
        }, res[-1:0], res[[1]])

        res <- lapply(res, list)
        res
    }

    .listed_mo <- function(mo, n) {
        res <- lapply(mo, function(x) {
            if (!is.list(x)) {
                res <- list(list(x))
                names(res) <- n
            } else if (is.null(names(x))) {
                res <- list(x)
                names(res) <- n
            } else if (any(grepl(".+:.+", names(x)))) {
                res <- .grouped_mo(x, names(x))
            } else {
                res <- .nested_mo(x, names(x))
            }
            res
        })

        res <- Reduce(function(x, y) {
            mapply(c, x, y, SIMPLIFY = FALSE)
        }, res[-1:0], res[[1]])

        res <- lapply(res, list)
        res
    }

    .nested_mo <- function(mo, n) {
        names(mo) <- NULL

        res <- mapply(function(x, y) {
            if (!is.list(x)) {
                res <- list(list(x))
                if (length(x) > 1)
                    res <- list(res)
                names(res) <- y
            } else if (is.null(names(x))) {
                res <- .listed_mo(x, y)
            } else if (any(grepl(".+:.+", names(x)))) {
                res <- .grouped_mo(x, names(x))
            } else {
                res <- .nested_mo(x, names(x))
            }
            res
        }, mo, n, SIMPLIFY = FALSE)

        res <- unlist(res, recursive = FALSE)
        res
    }

    n <- names(mo)
    if (is.null(n)) {
        res <- .listed_mo(mo, "value")
    } else if (any(grepl(".+:.+", n))) {
        res <- .grouped_mo(mo, n)
    } else {
        res <- .nested_mo(mo, n)
    }

    res <- unlist(res, recursive = FALSE)

    len <- max(sapply(res, function(x) length(x[[1]])))
    res <- lapply(res, function(x) {
        if (!is.list(x)) {
            rep(x, each = len)
        } else {
            unlist(lapply(x, function(y) {
                if (length(y) == 1) {
                    rep(y, len)
                } else if (length(y) == len) {
                    unlist(y, recursive = FALSE)
                } else {
                    stop("The 'list model object' is not conversible to data frame.")
                }
            }), recursive = FALSE)
        }
    })

    res <- tryCatch(
        tibble::as.tibble(res),
        error = function(e) {
            stop("The 'list model object' is not conversible to data frame.")
        })

    res
}
