#' @title Internal functions
#'
#' @name .select_prefix
#'
#' @description Returns a \code{logical} vector indicating which elements in
#' named list are selected.
#'
#' @param prefix   A \code{character} containing a name prefix to be filtered.
#' @param named_list   Any named \code{list} containing the data to be filtered.
#'
#' @return A \code{logical} vector.
#'
.select_prefix <- function(prefix, named_list) {

    if (is.null(prefix))
        return(rep(TRUE, length(named_list)))

    select <- grepl(paste0("^", prefix, ".*$"), names(named_list))
    return(select)
}

#' @title Internal functions
#'
#' @name .open_json
#'
#' @description Open a json file and return its structure as a \code{list}.
#'
#' @param location   A \code{character} text indicating a file path (local or in the web) to be opened.
#' @param cache   A \code{logical} indicating if result can be retrieved from cache.
#'
#' @return A \code{list} object containing the read data.
#'
.open_json <- function(location, cache) {

    if (cache && exists(location, .cache, inherits = FALSE))
        return(.cache[[location]]$content)

    con <- tryCatch(url(location), error = function(e) {
        tryCatch(file(location), error = function(e) {
            stop(sprintf("Invalid file location '%s'", location), call. = FALSE)
        })
    })

    result <- tryCatch(
        jsonlite::fromJSON(suppressWarnings(readLines(con, warn = FALSE)),
                           simplifyDataFrame = FALSE,
                           simplifyMatrix = FALSE),
        error = function(e) {

            stop(sprintf(paste("Error while opening JSON from '%s'.",
                               "Reported error: %s"), location, e$message), call. = FALSE)
        }, finally = close(con))

    if (cache)
        .cache[[location]] <- list(content = result, timestamp = Sys.time())

    return(result)
}

#' @title Internal functions
#'
#' @name .open_multiple_jsons
#'
#' @description Open a json file and return its structure as a \code{list}.
#'
#' @param locations   A \code{character} text indicating a file path (local or in the web) to be opened.
#' @param cache   A \code{logical} indicating if result can be retrieved from cache.
#' @param progress_bar   A \code{logical} indicating if a progress bar must be created.
#'
#' @return A \code{list} object containing the read data.
#'
.open_multiple_jsons <- function(locations, cache, progress_bar) {

    if (progress_bar)
        pb <- utils::txtProgressBar(max = length(locations), style = 3)

    res <- lapply(locations, function(location) {

        if (progress_bar)
            utils::setTxtProgressBar(pb = pb, value = utils::getTxtProgressBar(pb) + 1)

        .open_json(location = location, cache = cache)
    })

    if (progress_bar)
        close(pb)

    return(res)
}

#' @title Internal functions
#'
#' @name .save_json
#'
#' @description Save a \code{list} data as a json file.
#'
#' @param x      A \code{list} data to be saved.
#' @param file   A \code{character} text indicating a file path (local) to be saved.
#'
#' @return A \code{data.frame} object containing the metadata.
#'
.save_json <- function(x, file) {

    tryCatch(
        suppressWarnings(jsonlite::write_json(x = x, path = file, pretty = TRUE)),
        error = function(e) {

            stop(sprintf("Error while saving JSON file '%s'.", file), call. = FALSE)
        })

    return(TRUE)
}

#' #' @title Internal functions
#' #'
#' #' @description Open a brick file using \code{raster} package. If \code{file} is a url, it uses
#' #' GDAL driver \code{vsicurl}.
#' #'
#' #' @param file   A \code{character} text indicating a file path (local or in the web) to be opened.
#' #'
#' #' @return A \code{RasterBrick} object.
#' #'
#' .raster.open <- function(file) {
#'
#'     if (grepl("^https?://.+", file)) {
#'
#'         file = paste0("/vsicurl/", file)
#'     }
#'
#'     raster <- tryCatch(raster::brick(file, values = FALSE), error = function(e) NULL)
#'
#'     if (is.null(raster)) {
#'
#'         stop(sprintf("Error while opening raster '%s'.\nFile is unreachable.", file))
#'     }
#'
#'     return(brick)
#' }
#'
#' #' @title Internal functions
#' #'
#' #' @description Test if all files in a bricks \code{data.frame} are reachable.
#' #' A progress bar will be showed if the number of files in bricks is greater or equal
#' #' the value of \code{getOption("progress_bar", 10)}.
#' #'
#' #' @param files   A bricks \code{data.frame}.
#' #'
#' #' @return \code{TRUE} if all bricks are reachable.
#' #'
#' .raster.touch <- function(files) {
#'
#'     progress_bar <- length(files) >= getOption(.progress_bar_option, 10)
#'
#'     if (progress_bar) {
#'
#'         pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
#'     }
#'
#'     for (file in files) {
#'
#'         .raster.open(file)
#'
#'         if (progress_bar) {
#'             utils::setTxtProgressBar(pb = pb, value = utils::getTxtProgressBar(pb) + 1)
#'         }
#'     }
#'
#'     if (progress_bar) {
#'         close(pb)
#'     }
#'
#'     return(TRUE)
#' }

#' #' @title Internal functions
#' #'
#' #' @name .is_empty
#' #'
#' #' @description Checks if a value is empty. The meaning of empty state depends on \code{x} class.
#' #' If \code{x} is a \code{data.frame}, the object is empty if it has zero columns or zero rows.
#' #' If \code{x} is a \code{character} vector, it is empty if \code{x} has length zero, has at least one \code{NA},
#' #' or \code{""} value. If \code{x} is a \code{numeric} vector, it is empty if it has length zero or has
#' #' at least one \code{NA} as its element. If \code{x} is a \code{list}, it is empty if it has length zero.
#' #' Otherwise, it is empty if it is \code{NULL} or has length zero.
#' #'
#' #' @param x   Any object to be tested.
#' #'
#' #' @return \code{TRUE} if \code{x} is empty. \code{FALSE} otherwise.
#' #'
#' .is_empty <- function(x) {
#'     UseMethod(".is_empty", x)
#' }
#'
#' #' @title Internal functions
#' #'
#' #' @name .is_empty.default
#' #'
#' #' @param x   Any object to be tested.
#' #'
#' #' @return \code{TRUE} if \code{x} is \code{NULL}. \code{FALSE} otherwise.
#' #'
#' .is_empty.default <- function(x) {
#'     is.null(x) || length(x) == 0
#' }
#'
#' #' @title Internal functions
#' #'
#' #' @name .is_empty.data.frame
#' #'
#' #' @param x   A \code{data.frame} object to be tested.
#' #'
#' #' @return \code{TRUE} if \code{data.frame} has zero columns or zero rows. \code{FALSE} otherwise.
#' #'
#' .is_empty.data.frame <- function(x) {
#'     length(x) == 0 || nrow(x) == 0
#' }
#'
#' #' @title Internal functions
#' #'
#' #' @name .is_empty.character
#' #'
#' #' @param x   A \code{character} vector to be tested.
#' #'
#' #' @return \code{TRUE} if \code{x} has length zero, has at least one \code{NA}, or \code{""} value. \code{FALSE} otherwise.
#' #'
#' .is_empty.character <- function(x) {
#'     length(x) == 0 || any(is.na(x)) || any(trimws(x) == "")
#' }
#'
#' #' @title Internal functions
#' #'
#' #' @name .is_empty.numeric
#' #'
#' #' @param x   A \code{numeric} vector to be tested.
#' #'
#' #' @return \code{TRUE} if \code{x} has length zero or has at least one \code{NA}. \code{FALSE} otherwise.
#' #'
#' .is_empty.numeric <- function(x) {
#'     length(x) == 0 || any(is.na(x))
#' }
#'
#' #' @title Internal functions
#' #'
#' #' @name .is_empty.list
#' #'
#' #' @param x   A \code{list} to be tested.
#' #'
#' #' @return \code{TRUE} if \code{x} has length zero. \code{FALSE} otherwise.
#' #'
#' .is_empty.list <- function(x) {
#'     length(x) == 0
#' }

#' @title Internal functions
#'
#' @name .sublime_list
#'
#' @description List a human readable list of elements.
#'
#' @param x             A \code{character} vector to be listed.
#' @param conjunction   A \code{character} text to be used in output text.
#'
#' @return A \code{character} text.
#'
.sublime_list <- function(x, conjunction = "and") {

    if (length(x) == 0) {

        return("''")
    }

    if (length(x) == 1) {

        return(paste0("'", x, "'"))
    }

    sep_and <- if (length(x) > 2) sprintf(", %s ", conjunction) else sprintf(" %s ", conjunction)

    result <- paste0(paste0("'", x[1:(length(x) - 1)], "'", collapse = ", "),
                     paste0(sep_and, "'", x[length(x)], "'"))
    return(result)
}
