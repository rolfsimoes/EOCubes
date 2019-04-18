#' @title Cubes management functions
#'
#' @name make_stacks
#'
#' @description Returns a new \code{EOCubes_cube} data structure with filtered tiles and
#' fetches tiles data from remote.
#'
#' @param cube   A \code{EOCubes_cube} data structure.
#' @param bands   A \code{character} vector with band names to be retrieved.
#' @param start_dates   A \code{date} vector to filter layers by date.
#' @param end_dates   A \code{date} vector to filter layers by date.
#'
#' @return A \code{EOCubes_organized, EOCubes_cube} cube data structure.
#'
#' @export
#'
make_stacks <- function(cube, bands, start_dates, end_dates, count) {

    if (is.character(start_dates))
        start_dates <- as.Date(start_dates, "%Y-%m-%d")

    if (is.character(end_dates))
        end_dates <- as.Date(end_dates, "%Y-%m-%d")

    res <- lapply(cube$tiles, function(x) {

        if (!all(bands %in% names(x)))
            stop(sprintf("Informed band(s) %s not found in cube definition.",
                         .sublime_list(bands[!(bands %in% names(x$bands))])))

        lapply(x[bands], function(y) {

            timeline <- as.Date(y$date, "%Y-%m-%d")
            res <- lapply(seq_along(start_dates), function(i) {

                selected <- timeline >= start_dates[i] & timeline < end_dates[i]

                res <- list()
                if ((is.null(count) && sum(selected) > 0) || (!is.null(count) && sum(selected) == count))
                    res <- list(href = y$href[selected], date = timeline[selected])
                return(res)
            })

            res <- Filter(function(x) { length(x) > 0 }, res)

            names(res) <- sapply(res, function(x) {
                paste(min(x$date), max(x$date), sep = "_")
            })

            return(res)
        })
    })

    return(res)
}
