#' @importFrom jsonlite write_json fromJSON
#' @importFrom pbmcapply pbmclapply
#'
NULL

# global environment used for configuration
.global = new.env()
.cache = new.env()

# on load
.onLoad <- function(lib, pkg) {

    con <- file("~/.EOCubes/config.json")
    config(con) # close connection internally?
    save_config()
    load_cache()
}
