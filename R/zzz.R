#' @importFrom jsonlite write_json fromJSON
#'
NULL

# global environment used for configuration
.global = new.env()

# global entries
.remotes = "remotes"
.cubes = "cubes"

# constants files
.local_base   = "~/.EOCubes"

# default remotes
.default_remotes <- list(
    version = "0.6",
    default = "localhost",
    remotes = list(
        eocubes = list(
            description = "Cubes maintained and curated by EOCubes team.",
            keywords = c("EOCubes", "INPE"),
            href = "https://eocubes-test.s3.amazonaws.com/catalog.json"),
        localhost = list(
            description = "Local maintained cubes.",
            keywords = c("Local"),
            href = sprintf("%s/localhost/catalog.json", .local_base)
        )))

# load remotes definition
.load_remotes <- function() {

    if (!dir.exists(.local_base)) {

        message(sprintf("Creating the config directory '%s'.", .local_base))
        suppressWarnings(dir.create(.local_base, showWarnings = FALSE))
    }

    file <- sprintf("%s/remotes.json", .local_base)

    remotes <- tryCatch(
        .open_json(file),
        error = function(e) {

            message(sprintf(paste(
                "Error when trying to read remote list file '%s'.",
                "Loading default remote list."), file))
            return(.default_remotes)
        })

    .global[[.remotes]] <- remotes

    tryCatch(
        .save_json(.global[[.remotes]], file),

        error = function(e) {

            message(sprintf(paste(
                "Error when trying to save remotes list file '%s'.",
                "Continuing anyway."), file))
        })

    invisible(TRUE)
}

# on load
.onLoad <- function(lib, pkg) {

    .load_remotes()
}
