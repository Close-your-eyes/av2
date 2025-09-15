#' Concatenate files
#'
#' Must be files of same type in folder x, named in sequential order.
#'
#' @param x directory with (video) files
#' @param out_path output path, defaults to x
#' @param out_name output name, defaults to 'firstfilename'___'lastfilename'
#' @param overwrite overwrite output file?
#'
#' @returns nothing
#' @export
#'
#' @examples
video_concat <- function(x,
                         out_path = NULL,
                         out_name = NULL,
                         overwrite = F) {

    if (!requireNamespace("brathering", quietly = T)) {
        devtools::install_github("Close-your-eyes/brathering")
    }

    # https://trac.ffmpeg.org/wiki/Concatenate
    # procedure = c("demuxer", "protocol")

    if (!file.exists(x)) {
        stop(x, " not found.")
    }
    if (!fs::is_dir(x)) {
        stop("x must be a directory.")
    }
    out_path <- ifelse(is.null(out_path), x, suppressWarnings(normalizePath(out_path)))

    vidfiles <- list.files(x, full.names = T)
    if (!length(vidfiles)) {
        stop("no files found.")
    }
    ext <- tools::file_ext(vidfiles)
    ext <- names(which.max(table(ext)))
    vidfiles <- vidfiles[which(grepl(paste0(ext, "$"), vidfiles))]

    vidfiles_ts <- paste0(tools::file_path_sans_ext(vidfiles), ".ts")

    if (is.null(out_name)) {
        out_name <- paste0(tools::file_path_sans_ext(basename(vidfiles[1])), "___",
                           tools::file_path_sans_ext(basename(vidfiles[length(vidfiles)])),
                           ".", tools::file_ext(vidfiles[1]))
    }
    if (tools::file_ext(out_name) == "") {
        out_name <- paste0(out_name, ".", tools::file_ext(vidfiles[1]))
    }
    if (tools::file_ext(out_name) != tools::file_ext(vidfiles[1])) {
        stop("out_name must have file extension of input files: ", tools::file_ext(vidfiles[1]))
    }
    output <- fs::path(out_path, out_name)

    if (!overwrite) {
        output <- brathering::make_filepath_unique(output)
    }
    overwrite <- ifelse(overwrite, "-y", "-n")

    purrr::map2(vidfiles, vidfiles_ts, ~system(paste0("ffmpeg -i ", .x, " -c copy ", .y), intern = T))
    system(paste0("ffmpeg -i 'concat:", paste(vidfiles_ts, collapse = "|"),"' -c copy ", overwrite, " ", output), intern = T)
    y <- file.remove(vidfiles_ts)
}
