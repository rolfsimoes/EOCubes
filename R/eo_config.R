default_config <- function() {

    res <- tryCatch(file("~/.EOCubes/config.json"),
                    error = function(e) {

                        file.create()
                    }
    )
    return(res)
}

load_config <- function(con) {

    .global[["conf"]] <- cast(open_json(con), type = "eo_config")

    invisible(NULL)
}

save_config <- function(con) {

    save_json(.global[["conf"]], con = con)

    invisible(NULL)
}

add_provider <- function(pr, name) {

    add_entry(.global[["conf"]], pr = pr, name = name)

    invisible(NULL)
}

del_provider <- function(pr, name) {

    del_entry(.global[["conf"]], name = name)

    invisible(NULL)
}

list_providers <- function() {

    return(list_entries(.global[["conf"]]))
}
