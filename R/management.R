#' @title Coverage management functions
#'
#' @name get_coverage
#'
#' @description Read a \code{coverage} object from remotes.
#'
#' @param name   A \code{character} text with coverage name.
#'
#' @return A \code{coverage} object.
#'
#' @export
#'
get_coverage <- function(name) {


    if (!(name %in% names(.global[["remote-manifest"]]))) {

        stop(sprintf("Coverage '%s' not found.", name))
    }

    coverage <- .open_yml(
        .global[["remote-manifest"]][[name]][["definition"]]
    )

    .check_coverage(coverage)

    class(coverage) <- "coverage"

    return(coverage)
}

#' @title Coverage management functions
#'
#' @name add_coverage
#'
#' @description Register a coverage object in the local coverage list.
#'
#' @param coverage    A \code{coverage} object.
#' @param name        A \code{character} text with coverage name.
#' @param overwrite   A \code{logical} value indicating if coverage can be overwritten. Default \code{FALSE}.
#'
#' @return \code{TRUE} if coverage is registered.
#'
#' @export
#'
add_coverage <- function(coverage, name, overwrite = FALSE) {

    if (class(coverage) == "character") {

        name <- coverage
        coverage <- get_coverage(name)
    }

    .check_coverage(coverage)

    class(coverage) <- "coverage"

    if (!is.null(.global[["manifest"]][[name]]) && !overwrite) {

        stop(sprintf(paste(
            "There is already a registered (local) coverage",
            "with the name '%s'.", name
        )))
    }

    file <- tempfile(name, .config.base, ".yml")
    tryCatch(
        .save_yml(coverage, file),
        error = function(e) {

            stop(sprintf("Error while saving coverage in file '%s'.", file))
        })

    .global[["manifest"]][[name]] <- .set_definition.manifest(
        manifest   = coverage[["manifest"]],
        definition = file
    )

    file <- .manifest_yml(.config.base)
    tryCatch(
        .save_yml(.global[["manifest"]], file),
        error = function(e) {

            message(sprintf(paste(
                "Error while saving coverage '%s' manifest file.",
                "Continuing anyway."), file
            ))
        })

    .load_coverages(.global[["manifest"]][name])

    invisible(TRUE)
}

#' @title Coverage management functions
#'
#' @name describe_coverage
#'
#' @description Describes a given coverage by name. The name is searched first in
#' the local manifest list file. If not found, then it is searched in remote
#' manifests file. The function returns the manifest of the corresponing coverage name.
#'
#' @param name   A \code{character} with the coverage name.
#'
#' @return A coverage manifest \code{list}. If the informed name does not
#' correspond to any coverage, return \code{NULL}.
#'
#' @export
#'
describe_coverage <- function(name) {

    result <- .global[["manifest"]][[name]]

    if (is.null(result)) {

        result <- .global[["remote-manifest"]][[name]]
    }

    if (is.null(result)) {

        stop(sprintf("Coverage '%s' not found.", name))
    }

    return(result)
}

#' @title Coverage management functions
#'
#' @name list_coverages
#'
#' @description List all registered and remote coverage manifests.
#'
#' @param local_only   A \code{logical} informing if only local registered coverage
#'                     manifests must be returned.
#'
#' @return a named \code{list} with a brief description of all available coverages.
#'
#' @export
#'
list_coverages <- function(local_only = FALSE) {

    result <- list("local" = lapply(.global[["manifest"]], function(x) {

        x[["description"]]
    }))

    if (!local_only) {

        result <- append(result, list(
            "remote" = lapply(.global[["remote-manifest"]], function(x) {

                x[["description"]]
            })))
    }

    return(result)
}

#' @title Coverage management functions
#'
#' @name rm_coverage
#'
#' @description Unregister a coverage object from the local manifest list file.
#'
#' @param name       A \code{character} text with coverage name.
#'
#' @return \code{TRUE} if coverage is unregistered.
#'
#' @export
#'
rm_coverage <- function(name) {

    if (is.null(.global[["manifest"]][[name]])) {

        stop(sprintf("The coverage '%s' was not found.", name))
    }

    .global[["manifest"]] <-
        .global[["manifest"]][names(.global[["manifest"]]) != name]

    file <- .manifest_yml(.config.base)
    tryCatch(
        .save_yml(.global[["manifest"]], file),
        error = function(e) {
            message(sprintf(paste(
                "Error while saving coverage '%s' manifest file.",
                "Continuing anyway."),
                file))
        })

    .global[["coverages"]] <-
        .global[["coverages"]][names(.global[["coverages"]]) != name]

    message(sprintf("The coverage '%s' has been removed.", name))

    invisible(TRUE)
}

#' @title Coverage management functions
#'
#' @name add_remote
#'
#' @description Add a new remote to the local remotes list.
#'
#' @param name        A \code{character} text with the remote name.
#' @param location    A \code{character} text with a file path or url to a YAML manifest list file.
#' @param bucket      An optional \code{character} text with bucket AmazonS3 name. Default \code{NULL}.
#' @param overwrite   A \code{logical} value indicating if remote can be overwritten. Default \code{FALSE}.
#' @param ...         Any extra arguments to be passed to \code{aws.s3} package to access the bucket.
#'
#' @return \code{TRUE} if a remote is added.
#'
#' @export
#'
add_remote <- function(name, location, bucket = NULL, overwrite = FALSE, ...) {

    if (!is.null(.global[["remotes"]][[name]]) && !overwrite) {

        stop(sprintf("Remote name '%s' already exists.", name))
    }

    tryCatch({
        if (!is.null(bucket) && !invisible(aws.s3::bucket_exists(bucket))) {

            message(sprintf(paste(
                "Informed bucket '%' is not accessible with current authentication keys.",
                "Continuing anyway."), bucket))
        }}, error = function(e) {

            stop(sprintf(paste(
                "Error while checking bucket '%s'.",
                "Error message:\n",
                "\"%s\""), bucket, e$message))
        }
    )

    manifest <- .open_yml(.manifest_yml(location))

    .check_manifest(manifest)

    .global[["remotes"]][[name]] <- list("location" = location, "bucket" = bucket)

    file <- .remotes_yml(.config.base)
    tryCatch(
        .save_yml(.global[["remotes"]], file),
        error = function(e) {

            message(sprintf(paste(
                "Error while saving remotes file '%s'.",
                "Continuing anyway."), file))
        })

    .load_remotes()

    invisible(TRUE)
}

#' @title Coverage management functions
#'
#' @name list_remotes
#'
#' @description Lists all registered remotes.
#'
#' @return A remote \code{list}.
#'
#' @export
#'
list_remotes <- function() {

    result <- .global[["remotes"]]

    return(result)
}

#' @title Coverage management functions
#'
#' @name rm_remote
#'
#' @description Removes a remote from the local remote list.
#'
#' @param name   A \code{character} text with the remote name to be removed.
#'
#' @return \code{TRUE} if a remote is removed.
#'
#' @export
#'
rm_remote <- function(name) {

    if (is.null(.global[["remotes"]][[name]])) {

        stop(sprintf("Remote name '%s' does not exist.", name))
    }

    .global[["remotes"]] <-
        .global[["remotes"]][names(.global[["remotes"]]) != name]

    file <- .remotes_yml(.config.base)
    tryCatch(yaml::write_yaml(.global[["remotes"]][["location"]], file),
             error = function(e) {

                 message(sprintf(paste(
                     "Error while saving remotes file '%s'.",
                     "Continuing anyway."), file))
             })

    .load_remotes()

    return(TRUE)
}



.load_local <- function() {

    if (!dir.exists(.config.base)) {

        message(sprintf("Creating the config directory '%s'.", .config.base))
        suppressWarnings(dir.create(.config.base, showWarnings = FALSE))
    }

    file <- .manifest_yml(.config.base)

    manifest <- tryCatch(
        .open_yml(file),
        error = function(e) {

            message(sprintf(paste(
                "The manifest file '%s' is missing or unreachable.",
                "Loading default."), file))
            return(.manifest.default)
        }
    )

    tryCatch(
        .save_yml(manifest, file),
        error = function(e) {

            message(sprintf(paste(
                "Error when trying to save coverage manifest file '%s'.",
                "Continuing anyway."), file))
        }
    )

    .global[["manifest"]] <- manifest

    .load_coverages(manifest)

    return(TRUE)
}

.load_remotes <- function() {

    if (!dir.exists(.config.base)) {

        message(sprintf("Creating the config directory '%s'.", .config.base))
        suppressWarnings(dir.create(.config.base, showWarnings = FALSE))
    }

    file <- .remotes_yml(.config.base)

    remotes <- tryCatch(
        .open_yml(file),
        error = function(e) {

            message(sprintf(paste(
                "Error when trying to read remote list file '%s'.",
                "Loading default remote list."), file))
            return(.remotes.default)
        })

    .global[["remotes"]] <- remotes

    tryCatch(
        .save_yml(remotes, file),

        error = function(e) {

        message(sprintf(paste(
            "Error when trying to save remotes list file '%s'.",
            "Continuing anyway."), file))
    })

    .load_manifest(remotes)

    return(TRUE)
}

.load_manifest <- function(remotes) {

    new_remote_manifest <- list()

    for (i in names(remotes)) {

        location <- .manifest_yml(remotes[[i]][["location"]])
        manifest <- tryCatch(
            .open_yml(location),
            error = function(e) {

                message(sprintf(paste(
                    "The manifest file '%s' is missing or unreachable.",
                    "Skipping file."), location))
                return(list())
            }
        )

        for (j in names(manifest)) {

            new_remote_manifest[[j]] <- manifest[[j]]
        }
    }

    .global[["remote-manifest"]] <- new_remote_manifest

    return(TRUE)
}

.load_coverages <- function(manifest) {

    coverages <- lapply(manifest, function(entry) {

        location <- entry[["definition"]]
        coverage <- tryCatch(
            .open_yml(location),
            error = function(e) {

                message(sprintf(paste(
                    "The coverage file '%s' is missing or unreachable.",
                    "Skipping file."), location))
                return(NULL)
            }
        )

        .check_coverage(coverage)

        class(coverage) <- "coverage"

        return(coverage)
    })

    coverages <- coverages[!is.null(coverages)]

    for (i in names(coverages)) {

        .global[["coverages"]][[i]] <- coverages[[i]]
    }

    return(TRUE)
}

#' @title Coverage management functions
#'
#' @name read_coverage
#'
#' @description Read a \code{coverage} object from YAML file.
#'
#' @param location   A \code{character} text indicating a file path or url to a valid YAML coverage definition.
#'
#' @return A \code{coverage} object read from YAML file definition.
#'
#' @export
#'
read_coverage <- function(location) {

    coverage <- .open_yml(location)

    .check_coverage(coverage)

    class(coverage) <- "coverage"

    return(coverage)
}

#' @title Coverage management functions
#'
#' @name save_coverage
#'
#' @description Save a \code{coverage} object to a YAML
#'
#' @param coverage   A \code{coverage} object.
#' @param file       A \code{character} text containg YAML file paths.
#'
#' @return \code{TRUE} if coverage is saved.
#'
#' @export
#'
save_coverage <- function(coverage, file) {

    tryCatch(yaml::write_yaml(x = coverage, file = file),
             error = function(e) {

                 stop(sprintf("Error while saving coverage in file '%s'.", file))
             })

    invisible(TRUE)
}

#' @title Coverage management functions
#'
#' @name publish_coverage
#'
#' @description Save a \code{coverage} object to a YAML in a remote S3 server
#' This function uses \code{aws.s3} package to access the remote server.
#' You must provide appropriate access key to be able to write in the bucket.
#'
#' @param coverage    A \code{coverage} object.
#' @param name        A \code{character} text with coverage name.
#' @param remote      A \code{character} text with a valid remote server (with 'bucket' defined).
#' @param overwrite   A \code{logical} indicating if coverage can be overwritten in bucket. Default \code{FALSE}.
#' @param ...         Any extra arguments to be passed to \code{aws.s3} package to access the bucket.
#'
#' @return \code{TRUE} if coverage is published.
#'
#' @export
#'
publish_coverage <- function(coverage, name, remote, overwrite = FALSE, ...) {

    .check_coverage(coverage)

    .check_remote_bucket(remote, ...)

    bucket <- .global[["remotes"]][[remote]][["bucket"]]

    manifest <- tryCatch({
        if (invisible(aws.s3::head_object(.manifest.file, bucket = bucket))) {

            yaml::yaml.load(rawToChar(
                invisible(aws.s3::get_object(.manifest.file, bucket = bucket))
            ))
        } else {

            message(sprintf(
                "The bucket has no '%s' file. A new one will be created.", .manifest.file))

            .manifest.default
        }},
        error = function(e) {

            stop(sprintf(paste(
                "Error while fetching manifest file from bucket '%s'.",
                "Error message:\n",
                "\"%s\""), bucket, e$message))
        })

    if (!is.null(manifest[[name]]) && !overwrite) {

        stop(sprintf(paste(
            "There is already a registered coverage",
            "with the name '%s' in bucket '%s'.", name, bucket
        )))
    } else if (is.null(manifest[[name]])) {

        message(sprintf("Creating new coverage '%s'...", name))
    } else if (overwrite) {

        message(sprintf("Overwriting the coverage '%s'...", name))
    }

    file <- sprintf("%s.yml", name)
    temp_file <- tempfile(tmpdir = .config.base, "")

    manifest[[name]] <- .set_definition.manifest(
        coverage[["manifest"]], definition = .s3_url(bucket, object = file))


    tryCatch({
        .save_yml(manifest, temp_file)

        if (!invisible(aws.s3::put_object(
            file = temp_file, object = .manifest.file, bucket = bucket,
            acl = "public-read", ...))) {

            stop(sprintf("Error while saving manifest file '%s'.", .manifest.file))
        }

        file.remove(temp_file)

        .save_yml(coverage, temp_file)

        if (!invisible(aws.s3::put_object(
            file = temp_file, object = file, bucket = bucket,
            acl = "public-read", ...))) {

            stop(sprintf("Error while saving coverage definition file '%s'.", file))
        }

        file.remove(temp_file)
    }, error = function(e) {

        stop(sprintf(paste(
            "Error while registering coverage in bucket '%s'.",
            "Error message:\n",
            "\"%s\""), bucket, e$message))
    })

    .load_manifest(.global[["remotes"]])

    message(sprintf("Coverage published in remote '%s'.", remote))

    invisible(TRUE)
}

#' @title Coverage management functions
#'
#' @name unpublish_coverage
#'
#' @description Removes a \code{coverage} object from a remote S3 server
#' This function uses \code{aws.s3} package to access the remote server.
#' You must provide appropriate access key to be able to write in the bucket.
#'
#' @param name     A \code{character} text with the coverage name to be removed.
#' @param remote   A \code{character} text with a valid remote server (with 'bucket' defined).
#' @param ...      Any extra arguments to be passed to \code{aws.s3} package to access the bucket.
#'
#' @return \code{TRUE} if coverage is published.
#'
#' @export
#'
unpublish_coverage <- function(name, remote, ...) {

    .check_remote_bucket(remote, ...)

    bucket <- .global[["remotes"]][[remote]][["bucket"]]

    manifest <- tryCatch(
        if (invisible(aws.s3::head_object(.manifest.file, bucket = bucket))) {

            yaml::yaml.load(rawToChar(
                invisible(aws.s3::get_object(.manifest.file, bucket = bucket))
            ))
        },
        error = function(e) {

            stop(sprintf(paste(
                "Error while fetching manifest file from bucket '%s'.",
                "Error message:\n",
                "\"%s\""), bucket, e$message))
        })

    if (is.null(manifest[[name]])) {

        stop(sprintf(paste(
            "There is no registered coverage",
            "with the name '%s' in bucket '%s'.", name, bucket
        )))
    }

    temp_file <- tempfile(tmpdir = .config.base, "")

    manifest <- manifest[names(manifest) != name]

    tryCatch({
        .save_yml(manifest, temp_file)

        if (!invisible(aws.s3::put_object(
            file = temp_file, object = .manifest.file, bucket = bucket,
            acl = "public-read", ...))) {

            stop(sprintf("Error while saving manifest file '%s'.", .manifest.file))
        }

        file.remove(temp_file)

    }, error = function(e) {

        stop(sprintf(paste(
            "Error while registering coverage in bucket '%s'.",
            "Error message:\n",
            "\"%s\""), bucket, e$message))
    })

    .load_manifest(.global[["remotes"]])

    message(sprintf("Coverage unpublished from remote '%s'.", remote))

    invisible(TRUE)
}


.check_remote_bucket <- function(name, ...) {

    if (is.null(.global[["remotes"]][[name]])) {

        stop(sprintf("There is no registered remote with the name '%s'.", name))
    }

    bucket <- .global[["remotes"]][[name]][["bucket"]]
    if (is.null(bucket)) {

        stop(sprintf("The informed remote '%s' has no 'bucket' defined.", name))
    }

    tryCatch({
        if (!invisible(aws.s3::bucket_exists(bucket, ...))) {

            stop(sprintf(
                "The remote bucket '%s' is not accessible with current authentication key.", name)
            )
        }},
        error = function(e) {

            stop(sprintf(paste(
                "Error while checking bucket '%s'.",
                "Error message:\n",
                "\"%s\""), bucket, e$message))
        })

    return(TRUE)
}
