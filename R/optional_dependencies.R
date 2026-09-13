.av2_pak_packages <- c(
    brathering = "close-your-eyes/brathering"
)

.av2_cran_packages <- c(
    "lubridate",
    "magick",
    "pak",
    "stringi",
    "vroom"
)

.av2_optional_packages <- c(
    names(.av2_pak_packages),
    .av2_cran_packages
)

.ensure_package <- function(package) {
    if (!package %in% .av2_optional_packages) {
        stop("Unknown optional package: ", package, call. = FALSE)
    }

    if (requireNamespace(package, quietly = TRUE)) {
        return(invisible(TRUE))
    }

    message("Installing optional package '", package, "'.")
    if (package %in% names(.av2_pak_packages)) {
        if (!requireNamespace("pak", quietly = TRUE)) {
            utils::install.packages("pak")
        }
        if (!requireNamespace("pak", quietly = TRUE)) {
            stop(
                "Package 'pak' is required to install '", package, "'.",
                call. = FALSE
            )
        }
        pak::pak(unname(.av2_pak_packages[[package]]))
    } else {
        utils::install.packages(package)
    }

    if (!requireNamespace(package, quietly = TRUE)) {
        stop(
            "Package '", package,
            "' is required for this operation and could not be installed.",
            call. = FALSE
        )
    }

    invisible(TRUE)
}

.ensure_packages <- function(packages) {
    invisible(lapply(unique(packages), av2:::.ensure_package))
}
