default_config <- function() {

    res <- cast(list(version = "0.7",
                     remotes = list()), type = "eo_config")

    return(res)
}

config <- function(...) {

    UseMethod("config")
}

config.default <- function() {

    con <- new_connection("~/.EOCubes/config.json")

    res <- config(con)

    close(con)

    return(res)
}

config.connection <- function(con) {

    value <- tryCatch(open_json(con),
                      error = function(e) {

                          default <- default_config()
                          save_json(default, con = con)
                          return(default)
                      })

    .global[["conf"]] <- cast(value, default_type = "eo_config")

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

    if (!exists_item(cf, name))
        stop(sprintf("Provider entry '%s' not found.", name), call. = FALSE)

    res <- open_entry(cast_entry(cf$remotes[[name]], default_type = "eo_provider_0.7"))

    return(res)
}
