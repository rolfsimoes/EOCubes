#' @title Manifest functions
#'
#' @name manifest
#'
#' @description Generates a list with coverage description attributes.
#' This list is used to describe the data, the authors, the data policy,
#' relevant references to data sources used, and the data maintainer.
#'
#' @param description   A \code{character} text describing the coverage.
#' @param authors       A \code{character} vector with coverage authors.
#' @param references    A \code{character} vector with relevant references of data used.
#' @param citation      A \code{character} text with coverage citation.
#' @param data_policy   A \code{character} vector with urls or text informing data policy.
#' @param maintainer    A \code{character} text with coverage maintainer name and/or contact.
#' @param definition    A \code{character} text with a location of YML definition.
#'
#' @return A \code{list} containing all pairs \code{name = value} of coverage description attributes.
#'
#' @export
#'
manifest <- function(description = NA_character_,
                     authors     = NA_character_,
                     references  = NA_character_,
                     citation    = NA_character_,
                     data_policy = NA_character_,
                     maintainer  = NA_character_,
                     definition  = NA_character_) {

    result <- list(
        description = description,
        authors     = authors,
        references  = references,
        citation    = citation,
        data_policy = data_policy,
        maintainer  = maintainer,
        definition  = definition
    )

    return(result)
}

#' @title Internal manifest functions
#'
#' @name .set_definition.manifest
#'
#' @description Changes the location of file definition of the coverage.
#'
#' @param manifest     A coverage manifest \code{list}.
#' @param definition   A \code{character} text with a location of YML definition.
#'
#' @return A \code{list} containing all pairs \code{name = value} of coverage description attributes.
#'
.set_definition.manifest <- function(manifest, definition) {

    manifest[["definition"]] <- definition

    return(manifest)
}


#' @title Internal manifest functions
#'
#' @name .check_manifest
#'
#' @description Check for validity of a coverages manifest list.
#'
#' @param manifest   A manifest \code{list} object.
#'
#' @return \code{TRUE} if pass in all checks.
#'
.check_manifest <- function(manifest) {

    if (.is_empty(manifest)) {

        stop("The coverage manifest is empty.")
    }

    tryCatch(.as_lom(.as_df(manifest), .template.manifest),
             error = function(e) {

                 stop("Invalid coverage manifest data.")
             })

    return(TRUE)
}
