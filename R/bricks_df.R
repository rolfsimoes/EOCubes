
.filter_bands <- function(bricks_df, ...) {
    # get the list of all symbols in `...`
    # obs: named parameters in `...` are names in list
    dots <- as.list(substitute(list(...)))

    # start s as a list with the first element (call)
    s <- dots[1]

    # now, removes the first element of dots list
    dots <- dots[-1:0]

    # insert dots list in s list
    # the names of these elements are the same as the names of dots elements
    # unamed parameters are also appended
    if (length(dots) > 0) {
        if (!is.null(names(dots)))
            stop("Arguments must evaluate to logical values.")

        s[2:(length(dots) + 1)] <- sapply(dots, function(x) {

            if (is.name(x) && (deparse(x) %in% bricks_df[["band_long_name"]] ||
                               deparse(x) %in% bricks_df[["band_short_name"]])) {

                return(call("|", call("%in%", as.name("band_long_name"), deparse(x)),
                            call("%in%", as.name("band_short_name"), deparse(x))))
            }
            return(call("|", call("%in%", as.name("band_long_name"), x),
                        call("%in%", as.name("band_short_name"), x)))
        }, USE.NAMES = FALSE)
    } else return(bricks_df)

    # create a call from list which first parameter is a call element
    s <- as.call(s)

    # evaluates the call on df columns
    # reduce the results by AND element-wise
    # finally returning only those final true rows
    return(bricks_df[Reduce(function(rhs, lhs) {
        if (!is.logical(rhs) || !is.logical(lhs))
            stop("Arguments must evaluate to logical values.")
        if (length(lhs) > 1 && length(lhs) != nrow(bricks_df))
            stop(sprintf("Length of logical index must be 1 or %s, not %s", nrow(bricks_df), length(lhs)))
        return(rhs | lhs)
    }, eval(s, bricks_df), FALSE),])
}
