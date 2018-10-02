#' @title Bricks \code{data.frame} function
#'
#' @name bricks_df
#'
#' @description Creates a new \code{bricks} object used to make coverages.
#' A \code{bricks} object contains four mandatory attributes: 'file', 'key', 'band', and 'timeline'.
#' The 'key' and 'band' attributes are extracted from file name.
#' A bricks 'key' is a name that identifies all bands files of one brick. The bricks keys
#' text are trimmed on the following default characters \code{getOption("trim_keys", "_. ")}.
#' A bricks 'band' is the name of the band.
#'
#' @param files      A \code{character} vector listing all raster files location to be used as bricks.
#' @param bands_df   A \code{data.frame} containing 'name', 'short_name', 'min', 'max',
#'                   'fill', and 'scale' columns for each band to be retrieved from \code{files}.
#'
#' @return A bricks \code{data.frame} object.
#'
#' @export
#'
bricks_df <- function(files, bands_df = EOCubes::MOD13Q1_bands) {

    index <- match(.files.bands(files, bands = bands_df), bands_df[["band_long_name"]])

    files <- files[!is.na(index)]
    index <- index[!is.na(index)]

    if (.is_empty(index)) {

        stop("No file bands information matched the bands defition.")
    }

    message("Retrieving brick(s) metadata...")

    metadata <- .files.metadata(files)

    message("Done.")

    bricks <- tibble::as_tibble(cbind(tibble::as_tibble(list(
        file     = files,
        key      = .files.keys(files = files, bands = bands_df))),
        metadata,
        bands_df[index,]
    ))

    .check_bricks_df(bricks)

    return(bricks)
}

#' @title Internal bricks \code{data.frame} functions
#'
#' @name .check_bricks_df
#'
#' @description Check if the bricks \code{data.frame} attribute values are consistent and
#' all files are reachable.
#' A progress bar will be showed if the number of files in bricks is greater or equal
#' the value of \code{getOption("progress_bar", 10)}.
#'
#' @param bricks         A bricks \code{data.frame}.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_bricks_df <- function(bricks) {

    if (.is_empty(bricks)) {

        stop("Informed bricks is empty.")
    }

    if (.is_empty(bricks[["file"]])) {

        stop("The bricks definition has one or more empty 'file' values.")
    }

    if (.is_empty(bricks[["key"]])) {

        stop("The bricks definition has one or more empty 'key' values.")
    }

    tryCatch(
        .as_lom(bricks, .template.bricks),
        error = function(e) {

            stop("Invalid brick data.")
        }
    )

    return(TRUE)
}

.attach_bands_info <- function(bricks_df, bands_df) {

    index <- match(bricks_df[["band_long_name"]], bands_df[["band_long_name"]])

    for (i in names(bands_df)) {

        bricks_df[[i]] <- bands_df[[i]][index]
    }

    return(bricks_df)
}

.filter_bands <- function(bricks_df, ...) {
    # get the list of all symbols in `...`
    # obs: named parameters in `...` are names in list
    dots <- as.list(substitute(list(...)))

    # start s as a list with the first element (call)
    s <- dots[1]

    # now, removes the first element of dots list
    dots <- dots[-1:0]

    # insert dots list in s list
    # the names of these elements are the same as the names of dots elements
    # unamed parameters are also appended
    if (length(dots) > 0) {
        if (!is.null(names(dots)))
            stop("Arguments must evaluate to logical values.")

        s[2:(length(dots) + 1)] <- sapply(dots, function(x) {

            if (is.name(x) && (deparse(x) %in% bricks_df[["band_long_name"]] ||
                               deparse(x) %in% bricks_df[["band_short_name"]])) {

                return(call("|", call("%in%", as.name("band_long_name"), deparse(x)),
                            call("%in%", as.name("band_short_name"), deparse(x))))
            }
            return(call("|", call("%in%", as.name("band_long_name"), x),
                        call("%in%", as.name("band_short_name"), x)))
        }, USE.NAMES = FALSE)
    } else return(bricks_df)

    # create a call from list which first parameter is a call element
    s <- as.call(s)

    # evaluates the call on df columns
    # reduce the results by AND element-wise
    # finally returning only those final true rows
    return(bricks_df[Reduce(function(rhs, lhs) {
        if (!is.logical(rhs) || !is.logical(lhs))
            stop("Arguments must evaluate to logical values.")
        if (length(lhs) > 1 && length(lhs) != nrow(bricks_df))
            stop(sprintf("Length of logical index must be 1 or %s, not %s", nrow(bricks_df), length(lhs)))
        return(rhs | lhs)
    }, eval(s, bricks_df), FALSE),])
}
