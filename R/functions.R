.remotes_yml <- function(base) {

    sprintf("%s/remotes.yml", base)
}

.manifest_yml <- function(base) {

    sprintf("%s/manifest.yml", base)
}

.coverage_yml <- function(base) {

    sprintf("%s/coverage.yml", base)
}

.open_yml <- function(location) {

    con <- tryCatch(url(location), error = function(e) {
        file(location)
    })

    result <- tryCatch(
        suppressWarnings(yaml::yaml.load(readLines(con, warn = FALSE))),
        error = function(e) {

            stop(sprintf(paste(
                "Error while opening YAML from '%s'. The file is unreachable."), location))
        }, finally = close(con))

    return(result)
}

.save_yml <- function(x, file) {

    tryCatch(
        suppressWarnings(yaml::write_yaml(x, file)),
        error = function(e) {

            stop(sprintf(
                "Error while saving YAML file '%s'.", file))
        })

    return(TRUE)
}

#' @title Internal functions
#'
#' @name .file.metadata
#'
#' @description Retrieve rater file geographycal metadata information.
#'
#' @param file   A \code{character} text indicating a file path (local or in the web) to be opened.
#'
#' @return A \code{data.frame} object containing the metadata.
#'
.file.metadata <- function(file) {

    brick <- .file.open(file = file)

    extent <- raster::extent(brick)

    result <-
        tibble::tibble(
            nrow      = raster::nrow(brick),
            ncol      = raster::ncol(brick),
            time_len  = raster::nlayers(brick),
            crs       = as.character(raster::crs(brick)),
            xmin      = extent@xmin,
            xmax      = extent@xmax,
            ymin      = extent@ymin,
            ymax      = extent@ymax,
            xres      = raster::xres(brick),
            yres      = raster::yres(brick),
            data_type = as.character(raster::dataType(brick)))

    return(result)
}

#' @title Internal functions
#'
#' @description Open a brick file using \code{raster} package. If \code{file} is a url, it uses
#' GDAL driver \code{vsicurl}.
#'
#' @param file   A \code{character} text indicating a file path (local or in the web) to be opened.
#'
#' @return A \code{RasterBrick} object.
#'
.file.open <- function(file) {

    if (grepl("^https?://.+", file)) {

        file = paste0("/vsicurl/", file)
    }

    brick <- tryCatch(raster::brick(file, values = FALSE), error = function(e) NULL)

    if (is.null(brick)) {

        stop(sprintf("Error while opening brick '%s'.\nFile is unreachable.", file))
    }

    return(brick)
}

#' @title Internal functions
#'
#' @name .files.bands
#'
#' @description Extracts from file names the name of the bands according to a bands definition.
#'
#' @param files   A \code{character} vector with all files paths.
#' @param bands   A \code{bands} object \code{data.frame} containing all bands definitions.
#'
#' @return A \code{character} vector with all bands corresponding to the input file names.
#'
.files.bands <- function(files, bands) {

    regex <-
        sprintf("^(.*\\/)?(.+)(%s)(.*)(\\..+)$",
                paste(bands[["band_long_name"]], collapse = "|"))

    result <- gsub(pattern = regex, replacement = "\\3", x = files)

    return(result)
}

#' @title Internal functions
#'
#' @name .files.keys
#'
#' @description Extracts from file names the bricks 'key' values. The 'key' groups
#' files of different bands into one bricks definition. According to
#' \code{getOption("trim_keys", "_. ")} option, the leading and trainling characters
#' will be trimmed of the key value.
#'
#' @param files   A \code{character} vector with all files paths.
#' @param bands   A \code{bands} object \code{data.frame} containing all bands definitions.
#'
#' @return A \code{character} vector with all keys corresponding to the input file names.
#'
.files.keys <- function(files, bands) {

    regex <-
        sprintf("^(.*\\/)?(.+)(%s)(.*)(\\..+)$",
                paste(bands[["band_long_name"]], collapse = "|"))

    result <- gsub(pattern = regex, replacement = "\\2\\4", x = files)

    trim <- getOption(.trim_keys_option, "_. ")

    if (nchar(trim) > 0) {

        regex <- sprintf("^[%s]*(.*[^_])[%s]*$",
                         trim, trim)

        result <- gsub(pattern = regex, replacement = "\\1", x = result)
    }

    return(result)
}

#' @title Internal functions
#'
#' @name .files.metadata
#'
#' @description Retrieve some metadata from the bricks' raster files.
#' A progress bar will be showed if the number of files in bricks is greater or equal
#' the value of \code{getOption("progress_bar", 10)}.
#'
#' @param files   A \code{character} vector with all files paths.
#'
#' @return A metadata \code{data.frame} with all metadata values.
#'
.files.metadata <- function(files) {

    progress_bar <- length(files) >= getOption(.progress_bar_option, 10)

    if (progress_bar) {

        pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
    }

    metadata <- lapply(
        files, function(file) {
            result <- .file.metadata(file = file)

            if (progress_bar) {
                utils::setTxtProgressBar(pb = pb, value = utils::getTxtProgressBar(pb) + 1)
            }

            return(result)
        })

    if (progress_bar) {
        close(pb)
    }

    metadata <- do.call(rbind, metadata)

    metadata <- tibble::as_tibble(metadata)

    return(metadata)
}

#' @title Internal functions
#'
#' @name .files.touch
#'
#' @description Test if all files in a bricks \code{data.frame} are reachable.
#' A progress bar will be showed if the number of files in bricks is greater or equal
#' the value of \code{getOption("progress_bar", 10)}.
#'
#' @param files   A bricks \code{data.frame}.
#'
#' @return \code{TRUE} if all bricks are reachable.
#'
.files.touch <- function(files) {

    progress_bar <- length(files) >= getOption(.progress_bar_option, 10)

    if (progress_bar) {

        pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
    }

    for (file in files) {

        .file.open(file)

        if (progress_bar) {
            utils::setTxtProgressBar(pb = pb, value = utils::getTxtProgressBar(pb) + 1)
        }
    }

    if (progress_bar) {
        close(pb)
    }

    return(TRUE)
}

#' @title Internal functions
#'
#' @name .is_empty
#'
#' @description Checks if a value is empty. The meaning of empty state depends on \code{x} class.
#' If \code{x} is a \code{data.frame}, the object is empty if it has zero columns or zero rows.
#' If \code{x} is a \code{character} vector, it is empty if \code{x} has length zero, has at least one \code{NA},
#' or \code{""} value. If \code{x} is a \code{numeric} vector, it is empty if it has length zero or has
#' at least one \code{NA} as its element. If \code{x} is a \code{list}, it is empty if it has length zero.
#' Otherwise, it is empty if it is \code{NULL} or has length zero.
#'
#' @param x   Any object to be tested.
#'
#' @return \code{TRUE} if \code{x} is empty. \code{FALSE} otherwise.
#'
.is_empty <- function(x) {
    UseMethod(".is_empty", x)
}

#' @title Internal functions
#'
#' @name .is_empty.default
#'
#' @param x   Any object to be tested.
#'
#' @return \code{TRUE} if \code{x} is \code{NULL}. \code{FALSE} otherwise.
#'
.is_empty.default <- function(x) {
    is.null(x) || length(x) == 0
}

#' @title Internal functions
#'
#' @name .is_empty.data.frame
#'
#' @param x   A \code{data.frame} object to be tested.
#'
#' @return \code{TRUE} if \code{data.frame} has zero columns or zero rows. \code{FALSE} otherwise.
#'
.is_empty.data.frame <- function(x) {
    length(x) == 0 || nrow(x) == 0
}

#' @title Internal functions
#'
#' @name .is_empty.character
#'
#' @param x   A \code{character} vector to be tested.
#'
#' @return \code{TRUE} if \code{x} has length zero, has at least one \code{NA}, or \code{""} value. \code{FALSE} otherwise.
#'
.is_empty.character <- function(x) {
    length(x) == 0 || any(is.na(x)) || any(trimws(x) == "")
}

#' @title Internal functions
#'
#' @name .is_empty.numeric
#'
#' @param x   A \code{numeric} vector to be tested.
#'
#' @return \code{TRUE} if \code{x} has length zero or has at least one \code{NA}. \code{FALSE} otherwise.
#'
.is_empty.numeric <- function(x) {
    length(x) == 0 || any(is.na(x))
}

#' @title Internal functions
#'
#' @name .is_empty.list
#'
#' @param x   A \code{list} to be tested.
#'
#' @return \code{TRUE} if \code{x} has length zero. \code{FALSE} otherwise.
#'
.is_empty.list <- function(x) {
    length(x) == 0
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
