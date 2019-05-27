#' @importFrom jsonlite write_json fromJSON
#' @importFrom pbmcapply pbmclapply
#'
NULL

# global environment used for configuration
.global = new.env()
.cache = new.env()

# global constant
.local_base = "~/.EOCubes"

# default remotes
.default_remotes <- list(
    version = "0.7",
    default = "localhost",
    remotes = list(
        eocubes = list(
            description = "Cubes maintained and curated by EOCubes team.",
            keywords = c("EOCubes", "INPE"),
            href = "https://eocubes-test.s3.amazonaws.com/catalog.json"),
        localhost = list(
            description = "Local maintained cubes.",
            keywords = c("Local"),
            href = path.expand(sprintf("%s/localhost/catalog.json", .local_base))
        )))

# load remotes definition
.load_remotes <- function() {

    base <- path.expand(.local_base)

    if (!dir.exists(base)) {

        message(sprintf("Creating the config directory '%s'.", base))
        suppressWarnings(dir.create(base, showWarnings = FALSE))
    }

    file <- sprintf("%s/root.json", base)

    remotes <- tryCatch(
        .open_json(file, cache = FALSE),
        error = function(e) {

            message(sprintf(paste(
                "Error when trying to read remote list file '%s'.",
                "Loading default remote list."), file))
            return(.default_remotes)
        })

    .global[["root"]] <- remotes

    tryCatch(
        .save_json(.global[["root"]], file),

        error = function(e) {

            message(sprintf(paste(
                "Error when trying to save remotes list file '%s'.",
                "Continuing anyway."), file))
        })

    invisible(TRUE)
}

.load_cache <- function() {

    base <- path.expand(.local_base)

    if (!dir.exists(base)) {

        message(sprintf("Creating the config directory '%s'.", base))
        suppressWarnings(dir.create(base, showWarnings = FALSE))
    }

    file <- sprintf("%s/cache.RData", base)

    if (file.exists(file))
        load(file, .cache)

    invisible(TRUE)
}

# on load
.onLoad <- function(lib, pkg) {

    .load_remotes()
    .load_cache()
}
