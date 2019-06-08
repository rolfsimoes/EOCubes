#' @importFrom jsonlite write_json fromJSON
#' @importFrom pbmcapply pbmclapply
#'
NULL

# global environment used for configuration
.global = new.env()
.cache = new.env()

# on load
.onLoad <- function(lib, pkg) {

    load_root()
    save_root()
    load_cache()
}
