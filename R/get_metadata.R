#' Title
#'
#' @param filepath
#' @param format
#'
#' @returns
#' @export
#'
#' @examples
get_metadata <- function(filepath,
                         format = c("flat",
                                    "json",
                                    "xml",
                                    "default",
                                    "csv")) {
    format <- rlang::arg_match(format)
    cmd <- paste0("ffprobe -v quiet -print_format ", format, " -show_format ", filepath)

    system(cmd, intern = T)
}

#' Get tag from a (video) file
#'
#' @param filepath video file on disk
#' @param tag tag name
#'
#' @returns tag value, character
#' @export
#'
#' @examples
get_metadata_tag <- function(filepath, tag = "comment") {
    meta <- get_metadata(filepath = filepath, format = "flat")
    tag <- meta[which(grepl(paste0("format.tags.", tag), meta, ignore.case = T))]
    tag <- strsplit(tag, "\"")[[1]][2]
    return(tag)
}

#' Get all the meta data of (video) files
#'
#' @param x file paths or directory
#' @param metadata_tag tag name
#'
#' @returns
#' @export
#'
#' @examples
get_metadata_extended <- function(x, metadata_tag = "comment") {
    x <- x_dir_or_files(x)

    comments <- purrr::map(stats::setNames(x, basename(x)), get_metadata_tag, tag = metadata_tag)
    cmddf <- purrr::map_dfr(comments, parse_cmd, .id = "file")
    infodf <- get_media_info(x) |>
        dplyr::mutate(attribute = ifelse(source != "duration", paste0(source, "_", attribute), attribute)) |>
        dplyr::select(-source) |>
        dplyr::rename("flag" = attribute)

    # sizedf <- stack(stats::setNames(file.info(x)$size, basename(x))) |>
    #     dplyr::rename("file" = ind, "value" = values) |>
    #     dplyr::mutate(flag = "filesize", file = as.character(file), value = as.character(value))
    df <- dplyr::bind_rows(cmddf, infodf) |>
        dplyr::mutate(flag = gsub("[:-]", "", flag)) |>
        tidyr::pivot_wider(names_from = flag, values_from = value)
    return(df)
}

#' Dissect ffmpeg command to its parts
#'
#' @param cmd ffmpeg command
#'
#' @returns data frame
#' @export
#'
#' @examples
parse_cmd <- function(cmd) {
    # Extract the program name (first token)
    program <- sub(" .*", "", cmd)

    # Remove program name from the rest
    cmd_args <- sub("^[^ ]+\\s+", "", cmd)

    # Regex to match flag-argument pairs (handles quoted values)
    matches <- gregexpr("(-\\S+)\\s+((?:'[^']*'|\"[^\"]*\"|[^-][^ ]*))", cmd_args, perl=TRUE)
    parts <- regmatches(cmd_args, matches)[[1]]

    # Extract flags and arguments
    flags <- sub("^(-\\S+)\\s+.*", "\\1", parts)
    arguments <- sub("^-\\S+\\s+", "", parts)

    # Remove surrounding quotes from arguments
    arguments <- gsub("^['\"]|['\"]$", "", arguments)

    # Combine program row + parsed flags
    df <- rbind(
        data.frame(flag = "program", value = program, stringsAsFactors = FALSE),
        data.frame(flag = flags, value = arguments, stringsAsFactors = FALSE)
    )

    return(df)
}
