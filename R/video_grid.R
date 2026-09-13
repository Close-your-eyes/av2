#' Arrange Multiple Videos in a Grid
#'
#' Build an FFmpeg command that places multiple videos in a regular grid. A
#' single row produces a side-by-side video. Inputs are scaled to fit a common
#' cell while preserving their aspect ratios, and any remaining space is filled
#' with `fill`.
#'
#' @param x A character vector of video file paths, or a directory containing
#'   video files. Directory entries are sorted by file name.
#' @param ncol,nrow Number of columns and rows. Supply either value, both values,
#'   or neither. When neither is supplied, a near-square layout is chosen. The
#'   grid must have room for every input.
#' @param scale Numeric vector `c(width, height)` giving the size of each grid
#'   cell in pixels. When `NULL`, the largest input width and height are used.
#' @param out_path Output directory. Defaults to the directory of the first
#'   input.
#' @param out_name Output file name, with or without an extension.
#' @param container Output container used when `out_name` has no extension.
#' @param fill FFmpeg color used for letterboxing and empty grid space.
#' @param audio Keep audio from the `"first"` input or discard it with
#'   `"none"`.
#' @param shortest Stop when the shortest input ends. When `FALSE`, FFmpeg uses
#'   the longest input.
#' @param encoder FFmpeg video encoder.
#' @param quality_crf Optional value for FFmpeg's `-crf` option.
#' @param preset Optional FFmpeg encoder preset.
#' @param fps_out Optional output frame rate.
#' @param overwrite Overwrite an existing output file? When `FALSE`, an
#'   existing output path is made unique.
#' @param flags_add_before_i Additional FFmpeg flags placed before the input
#'   arguments.
#' @param flags_add_after_i Additional FFmpeg flags placed after the generated
#'   mapping and encoding arguments.
#' @param which_ffmpeg FFmpeg executable name or path.
#' @param run_cmd Run the command? When `FALSE`, only return it.
#'
#' @return The FFmpeg command as a character string, invisibly when it is run.
#' @export
#'
#' @examples
#' \dontrun{
#' # Two videos side by side
#' video_grid(
#'   c("left.mp4", "right.mp4"),
#'   nrow = 1,
#'   out_name = "side_by_side.mp4"
#' )
#'
#' # Build (but do not run) a 2 x 2 grid command
#' video_grid(
#'   c("one.mp4", "two.mp4", "three.mp4", "four.mp4"),
#'   ncol = 2,
#'   scale = c(640, 360),
#'   run_cmd = FALSE
#' )
#' }
video_grid <- function(x,
                       ncol = NULL,
                       nrow = NULL,
                       scale = NULL,
                       out_path = NULL,
                       out_name = "video_grid",
                       container = c("mp4", "mkv", "webm", "mov"),
                       fill = "black",
                       audio = c("first", "none"),
                       shortest = TRUE,
                       encoder = c("libx264", "libx265", "libvpx_vp9", "h264_videotoolbox"),
                       quality_crf = NULL,
                       preset = NULL,
                       fps_out = NULL,
                       overwrite = FALSE,
                       flags_add_before_i = "",
                       flags_add_after_i = "-pix_fmt yuv420p",
                       which_ffmpeg = "ffmpeg8",
                       run_cmd = TRUE) {

    container <- rlang::arg_match(container)
    audio <- rlang::arg_match(audio)
    encoder <- rlang::arg_match(encoder)

    files <- .video_grid_files(x)
    n_inputs <- length(files)
    if (n_inputs < 2L) {
        stop("x must contain at least two video files.", call. = FALSE)
    }

    ncol <- .video_grid_count(ncol, "ncol")
    nrow <- .video_grid_count(nrow, "nrow")
    if (is.null(ncol) && is.null(nrow)) {
        ncol <- ceiling(sqrt(n_inputs))
        nrow <- ceiling(n_inputs / ncol)
    } else if (is.null(ncol)) {
        ncol <- ceiling(n_inputs / nrow)
    } else if (is.null(nrow)) {
        nrow <- ceiling(n_inputs / ncol)
    }
    if (ncol * nrow < n_inputs) {
        stop("ncol * nrow must be at least the number of input videos.", call. = FALSE)
    }

    if (is.null(scale)) {
        dimensions <- lapply(files, function(path) {
            tryCatch({
                info <- dplyr::filter(av2::media_info(path), key2 %in% c("video_height", "video_width"))
                info <- stats::setNames(as.numeric(info$value), info$key)
                c(width = info[["width"]][[1]], height = info[["height"]][[1]])
            }, error = function(e) c(width = NA_real_, height = NA_real_))
        })
        dimensions <- do.call(rbind, dimensions)
        if (any(!is.finite(dimensions))) {
            stop("Could not determine all input dimensions; supply scale explicitly.", call. = FALSE)
        }
        scale <- apply(dimensions, 2L, max)
    }
    if (!is.numeric(scale) || length(scale) != 2L ||
        any(!is.finite(scale)) || any(scale <= 0)) {
        stop("scale must be a positive numeric vector of length two.", call. = FALSE)
    }
    scale <- as.integer(round(scale))
    scale <- scale + scale %% 2L
    cell_width <- scale[[1]]
    cell_height <- scale[[2]]

    if (!is.character(fill) || length(fill) != 1L || is.na(fill) ||
        !grepl("^[[:alnum:]#@.]+$", fill)) {
        stop("fill must be one FFmpeg color name or color value.", call. = FALSE)
    }
    if (!is.logical(shortest) || length(shortest) != 1L || is.na(shortest)) {
        stop("shortest must be TRUE or FALSE.", call. = FALSE)
    }
    if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
        stop("overwrite must be TRUE or FALSE.", call. = FALSE)
    }
    if (!is.logical(run_cmd) || length(run_cmd) != 1L || is.na(run_cmd)) {
        stop("run_cmd must be TRUE or FALSE.", call. = FALSE)
    }
    if (!is.null(quality_crf) &&
        (!is.numeric(quality_crf) || length(quality_crf) != 1L || !is.finite(quality_crf))) {
        stop("quality_crf must be NULL or one finite number.", call. = FALSE)
    }
    if (!is.null(fps_out) &&
        (!is.numeric(fps_out) || length(fps_out) != 1L || !is.finite(fps_out) || fps_out <= 0)) {
        stop("fps_out must be NULL or one positive finite number.", call. = FALSE)
    }
    if (!is.null(preset) &&
        (!is.character(preset) || length(preset) != 1L || is.na(preset) || !nzchar(preset))) {
        stop("preset must be NULL or one non-empty character value.", call. = FALSE)
    }

    ff <- unshquote(which_ffmpeg)
    if (!is.character(ff) || length(ff) != 1L || is.na(ff) || !nzchar(ff)) {
        stop("which_ffmpeg must be one executable name or path.", call. = FALSE)
    }
    if (run_cmd && !nzchar(Sys.which(ff)) && !file.exists(ff)) {
        stop("FFmpeg executable not found: ", ff, call. = FALSE)
    }

    if (is.null(out_path)) {
        out_path <- dirname(files[[1]])
    } else {
        if (!is.character(out_path) || length(out_path) != 1L || is.na(out_path)) {
            stop("out_path must be NULL or one path.", call. = FALSE)
        }
        out_path <- path.expand(unshquote(out_path))
    }
    dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

    if (!is.character(out_name) || length(out_name) != 1L || is.na(out_name) || !nzchar(out_name)) {
        stop("out_name must be one non-empty file name.", call. = FALSE)
    }
    out_name <- basename(unshquote(out_name))
    if (!nzchar(tools::file_ext(out_name))) {
        out_name <- paste0(out_name, ".", container)
    }
    output <- file.path(out_path, out_name)
    if (!overwrite && file.exists(output)) {
        av2:::.ensure_package("brathering")
        output <- brathering::make_filepath_unique(output)
    }

    input_args <- paste(vapply(files, function(path) {
        paste("-i", shQuote(path))
    }, character(1)), collapse = " ")

    fps_filter <- if (is.null(fps_out)) "" else paste0(",fps=", fps_out)
    input_filters <- vapply(seq_len(n_inputs) - 1L, function(i) {
        paste0(
            "[", i, ":v]setpts=PTS-STARTPTS", fps_filter,
            ",scale=", cell_width, ":", cell_height,
            ":force_original_aspect_ratio=decrease",
            ",pad=", cell_width, ":", cell_height,
            ":(ow-iw)/2:(oh-ih)/2:color=", fill,
            ",setsar=1[v", i, "]"
        )
    }, character(1))

    layout <- vapply(seq_len(n_inputs) - 1L, function(i) {
        column <- i %% ncol
        row <- i %/% ncol
        paste0(column * cell_width, "_", row * cell_height)
    }, character(1))
    labels <- paste0("[v", seq_len(n_inputs) - 1L, "]", collapse = "")
    stack_filter <- paste0(
        labels,
        "xstack=inputs=", n_inputs,
        ":layout=", paste(layout, collapse = "|"),
        ":fill=", fill,
        ":shortest=", as.integer(shortest),
        "[stacked]"
    )
    canvas_filter <- paste0(
        "[stacked]pad=", ncol * cell_width, ":", nrow * cell_height,
        ":0:0:color=", fill, "[vout]"
    )
    filter_graph <- paste(c(input_filters, stack_filter, canvas_filter), collapse = ";")

    map_args <- '-map "[vout]"'
    if (audio == "first") {
        map_args <- paste(map_args, "-map 0:a? -c:a aac")
    } else {
        map_args <- paste(map_args, "-an")
    }

    cmd_parts <- c(
        ff,
        if (overwrite) "-y" else "-n",
        flags_add_before_i,
        input_args,
        "-filter_complex", shQuote(filter_graph),
        map_args,
        "-c:v", encoder,
        if (!is.null(quality_crf)) paste("-crf", quality_crf),
        if (!is.null(preset)) paste("-preset", preset),
        if (!is.null(fps_out)) paste("-r", fps_out),
        if (audio == "first" && shortest) "-shortest",
        flags_add_after_i,
        shQuote(output)
    )
    cmd <- paste(cmd_parts[nzchar(cmd_parts)], collapse = " ")

    if (run_cmd) {
        message("ffmpeg cmd: ", cmd)
        status <- system(cmd)
        if (!identical(status, 0L)) {
            stop("FFmpeg failed with exit status ", status, ".", call. = FALSE)
        }
        return(invisible(cmd))
    }

    cmd
}


.video_grid_count <- function(x, name) {
    if (is.null(x)) {
        return(NULL)
    }
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 1 || x %% 1 != 0) {
        stop(name, " must be NULL or one positive whole number.", call. = FALSE)
    }
    as.integer(x)
}


.video_grid_files <- function(x) {
    if (!is.character(x) || !length(x) || anyNA(x)) {
        stop("x must be a non-empty character vector.", call. = FALSE)
    }
    x <- path.expand(unshquote(x))
    if (length(x) == 1L && dir.exists(x)) {
        video_extensions <- c(
            "3gp", "avi", "flv", "m2ts", "m4v", "mkv", "mov", "mp4",
            "mpeg", "mpg", "mts", "ogv", "ts", "webm", "wmv"
        )
        x <- list.files(x, full.names = TRUE)
        x <- x[tolower(tools::file_ext(x)) %in% video_extensions]
    }
    missing <- !file.exists(x)
    if (any(missing)) {
        stop("Input file not found: ", x[which(missing)[[1]]], call. = FALSE)
    }
    if (any(dir.exists(x))) {
        stop("x must contain file paths, or one directory path.", call. = FALSE)
    }
    if (!length(x)) {
        stop("No video files found.", call. = FALSE)
    }
    normalizePath(x, mustWork = TRUE)
}
