default_config <- function() {

    res <- new_eo_object(list(version = "0.7",
                              remotes = list()), type = "eo_config")

    return(res)
}

config <- function(...) {

    UseMethod("config")
}

config.default <- function() {

    con <- new_connection("~/.EOCubes/config.json")

    res <- config(con)

    return(res)
}

config.connection <- function(con) {

    .global[["conf"]] <- tryCatch(open_eo_object(con, type = "eo_config"),
                                  error = function(e) {

                                      default <- default_config()
                                      save_json(default, con = con)
                                      return(default)
                                  })

    invisible(NULL)
}

save_config <- function(con) {

    save_json(.global[["conf"]], con = con)

    invisible(NULL)
}

add_provider <- function(pr, name) {

    add_item(.global[["conf"]], pr = pr, name = name)

    invisible(NULL)
}

del_provider <- function(pr, name) {

    del_item(.global[["conf"]], name = name)

    invisible(NULL)
}

list_providers <- function() {

    return(list_items(.global[["conf"]]))
}

provider.eo_config <- function(cf, name) {

    res <- open_entry(cast_entry(get_item(cf, name), default_type = "eo_provider"))

    return(res)
}
