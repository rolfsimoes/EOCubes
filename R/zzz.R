#' @importFrom aws.s3 get_bucket
#' @importFrom tibble as_tibble
#' @importFrom raster brick
#' @importFrom yaml as.yaml write_yaml read_yaml
#'
NULL

# on load
.onLoad <- function(lib, pkg) {

    .load_remotes()
    .load_local()
}

# global environment used for configuration
.global = new.env()

# default remotes
.remotes.default <- list(
    "EOCubes" = list(
        "location" = "https://eocubes.s3.amazonaws.com",
        "bucket"   = "eocubes")
)

# default manifest
.manifest.default <- list()

# constants files
.config.base   = "~/.EOCubes"
.manifest.file = "manifest.yml"

# options character text
.progress_bar_option = "progress_bar"
.trim_keys_option    = "trim_keys"
.s3_base_url_option  = "s3_base_url"
