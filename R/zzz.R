#' @importFrom jsonlite write_json fromJSON
#' @importFrom pbmcapply pbmclapply
#'
NULL

# global environment used for configuration
.global = new.env()
.cache = new.env()

# global constant
.local_base = "~/.EOCubes"

# default repositories
.default_repositories <- list(
    version = "0.7",
    default = "localhost",
    repositories = list(
        eocubes = list(
            description = "Cubes maintained and curated by EOCubes team.",
            keywords = c("EOCubes", "INPE"),
            href = "https://eocubes-test.s3.amazonaws.com/catalog.json"),
        localhost = list(
            description = "Local maintained cubes.",
            keywords = c("Local"),
            href = path.expand(sprintf("%s/localhost/catalog.json", .local_base))
        )))

# load repositories definition
.load_repositories <- function() {

    base <- path.expand(.local_base)

    if (!dir.exists(base)) {

        message(sprintf("Creating the config directory '%s'.", base))
        suppressWarnings(dir.create(base, showWarnings = FALSE))
    }

    .load_root()
    .save_root()

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

    .load_repositories()
    .load_cache()
}
