#' @title Internal functions
#'
#' @name .select_prefix
#'
#' @description Returns a \code{logical} vector indicating which elements in
#' named list are selected.
#'
#' @param prefix   A \code{character} vector containing one or more name
#' prefix to be filtered.
#' @param named_list   Any named \code{list} containing the data to be filtered.
#'
#' @return A \code{logical} vector.
#'
.select_prefix <- function(prefix, named_list) {

    if (is.null(prefix))
        return(rep(TRUE, length(named_list)))

    select <- rep(FALSE, length(named_list))
    for (p in prefix) {

        select <- grepl(paste0("^", p, ".*$"), names(named_list)) | select
    }

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
