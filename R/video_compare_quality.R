#' Compare Video Quality Against a Reference File
#'
#' Uses FFmpeg to compare the first video stream of a converted file with the
#' corresponding stream of the original. Audio is ignored. No video is written.
#'
#' @param x path to the converted video file on disk
#' @param reference path to the original video file on disk
#' @param metrics quality metrics to calculate: any combination of `"ssim"`,
#' `"psnr"`, and `"vmaf"`; defaults to SSIM and PSNR
#' @param start start time in seconds in x; defaults to 0
#' @param start_reference corresponding start time in reference; defaults to start.
#' For an excerpt converted from the original, use start = 0 and
#' start_reference = the excerpt's start time in the original.
#' @param duration number of seconds to compare; NULL compares to the end of the
#' shorter video
#' @param which_ffmpeg which version to use; see [check_ffmpeg()]
#' @param run_cmd run FFmpeg (TRUE) or just return the command string (FALSE)
#'
#' @returns When run_cmd is TRUE, a list containing:
#' \describe{
#'   \item{summary}{A tibble with one row per metric: metric, unit, score,
#'   min, p05, max, and frames. SSIM and VMAF scores are means of the per-frame
#'   values. The PSNR score is FFmpeg's overall PSNR, calculated from pooled
#'   pixel errors rather than the mean of per-frame PSNR values. p05 is the
#'   fifth percentile of frame scores, using the nearest observed rank.}
#'   \item{frames}{A tibble with metric, frame (starting at 1), and score. Frame
#'   numbers are relative to the compared section.}
#'   \item{files}{A tibble with role, file, and size_megabyte. Sizes describe the
#'   entire files, including audio and other streams, even for excerpt comparisons.}
#'   \item{cmd}{The executed FFmpeg command string.}
#'   \item{log}{FFmpeg's console output.}
#' }
#' When run_cmd is FALSE, returns only the command string. Temporary score logs
#' are removed after execution; the returned tibbles retain their contents.
#'
#' @details
#' Higher scores indicate greater similarity to the reference. SSIM's maximum
#' is 1; identical decoded pixels give infinite PSNR. VMAF typically uses a
#' scale from 0 to 100, but identical inputs need not score exactly 100. There
#' is no universal score that guarantees a visually indistinguishable result.
#'
#' Files must contain the same scene sequence, at the same resolution and frame
#' rate. Known resolution or frame-rate differences raise an error. Start times
#' align matching excerpts; the function does not detect scene offsets, missing
#' frames, or variable-frame-rate timing differences. Frames are paired by order
#' after seeking, ignoring their original timestamps to avoid container timestamp
#' rounding differences. Comparison stops at the shorter input without repeating
#' its final frame.
#' A warning is issued when known remaining durations differ by more than one
#' frame. Inspect frame-level results and view the videos as well as comparing
#' overall scores.
#'
#' No resizing, tone mapping, or frame-rate conversion is applied. FFmpeg may
#' negotiate a common pixel format, which affects what the scores measure.
#' Different pixel formats or known color metadata produce a warning. VMAF
#' requires FFmpeg compiled with libvmaf and uses its default model. Known HDR
#' inputs are rejected for VMAF because the default model is intended for SDR.
#'
#' @seealso [video_to_video()], [media_info()]
#' @references <https://ffmpeg.org/ffmpeg-filters.html#libvmaf>
#' @export
#'
#' @examples
#' \dontrun{
#' result <- video_compare_quality("converted.mp4", "original.mp4")
#' result$summary
#' result$frames
#'
#' video_compare_quality(
#'     x = "converted.mp4",
#'     reference = "original.mp4",
#'     metrics = c("ssim", "psnr", "vmaf"),
#'     start = 60,
#'     duration = 20
#' )
#'
#' # Compare an excerpt with the matching section of the original
#' video_compare_quality(
#'     x = "excerpt.mp4",
#'     reference = "original.mp4",
#'     start_reference = 120,
#'     duration = 20
#' )
#' }
video_compare_quality <- function(x,
                                  reference,
                                  metrics = c("ssim", "psnr"),
                                  start = 0,
                                  start_reference = start,
                                  duration = NULL,
                                  which_ffmpeg = "ffmpeg8",
                                  run_cmd = T) {

    metrics <- rlang::arg_match(metrics, values = c("ssim", "psnr", "vmaf"), multiple = T)
    metrics <- unique(metrics)
    if (!length(metrics)) {
        stop("select at least one metric.")
    }
    if (!is.logical(run_cmd) || length(run_cmd) != 1 || is.na(run_cmd)) {
        stop("run_cmd must be TRUE or FALSE.")
    }
    for (i in c("start", "start_reference", "duration")) {
        value <- get(i)
        if (i == "duration" && is.null(value)) next
        if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
            value < 0 || (i == "duration" && value == 0)) {
            stop(i, " must be a finite number of seconds ",
                 ifelse(i == "duration", "greater than zero.", "at least zero."))
        }
    }
    paths <- lapply(list(x = x, reference = reference), function(y) {
        if (!is.character(y) || length(y) != 1 || is.na(y)) {
            stop("x and reference must each be a single file path.")
        }
        y <- path.expand(unshquote(y))
        if (!file.exists(y) || fs::is_dir(y)) stop(y, " is not a file.")
        normalizePath(y, winslash = "/", mustWork = T)
    })
    x <- paths$x
    reference <- paths$reference

    ffmpegs <- check_ffmpeg()
    if (!length(ffmpegs)) stop("FFmpeg not installed or not in PATH.")
    ff <- rlang::arg_match(which_ffmpeg, values = ffmpegs)
    ff <- unname(Sys.which(ff))
    if (!nzchar(ff)) stop(which_ffmpeg, " is not executable or not in PATH.")

    # Safe basenames avoid the extra escaping rules for FFmpeg filter paths.
    # The shell changes directory; R's working directory remains unchanged.
    stats_files <- stats::setNames(vapply(metrics, function(i) {
        tempfile(pattern = paste0("av2_", i, "_"), fileext = ".log")
    }, character(1)), metrics)
    n <- length(metrics)
    input_flags <- function(path, offset) {
        paste("-ss", format(offset, scientific = F, trim = T, digits = 15),
              if (is.null(duration)) "" else paste("-t", format(duration, scientific = F, trim = T, digits = 15)),
              "-i", shQuote(path))
    }
    branches <- function(input, prefix) {
        labels <- paste0("[", prefix, seq_len(n), "]", collapse = "")
        # Pair by frame index; e.g. MKV and MP4 can round timestamps differently.
        paste0("[", input, ":v:0]settb=1/1,setpts=N,",
               if (n == 1) "null" else paste0("split=", n), labels)
    }
    filters <- vapply(seq_along(metrics), function(i) {
        metric <- metrics[i]
        filter <- if (metric == "vmaf") "libvmaf" else metric
        options <- if (metric == "vmaf") "log_fmt=csv:log_path=" else "stats_file="
        paste0("[dist", i, "][ref", i, "]", filter, "=", options,
               basename(stats_files[i]), ":shortest=1:repeatlast=0[out", i, "]")
    }, character(1))
    vf_flag <- paste(c(branches(0, "dist"), branches(1, "ref"), filters), collapse = ";")
    maps <- paste(vapply(seq_len(n), function(i) {
        paste("-map", shQuote(paste0("[out", i, "]")))
    }, character(1)), collapse = " ")
    cmd <- paste(
        "(cd", shQuote(tempdir()), "&&", shQuote(ff), "-hide_banner -nostdin -nostats -v info",
        input_flags(x, start), input_flags(reference, start_reference),
        "-filter_complex", shQuote(vf_flag), maps, "-an -vsync 0 -f null -)"
    )
    if (!run_cmd) return(cmd)

    console_file <- tempfile(pattern = "av2_quality_", fileext = ".log")
    on.exit(unlink(c(stats_files, console_file)), add = T)
    available <- system(paste(shQuote(ff), "-hide_banner -filters 2>&1"), intern = T)
    required <- ifelse(metrics == "vmaf", "libvmaf", metrics)
    for (i in required) {
        if (!any(grepl(paste0("[[:space:]]", i, "[[:space:]]"), available))) {
            stop(which_ffmpeg, " does not provide the ", i, " filter.",
                 if (i == "libvmaf") " Use a build compiled with libvmaf, or select SSIM/PSNR.")
        }
    }
    if (!nzchar(Sys.which("ffprobe"))) stop("ffprobe not installed or not in PATH.")
    info <- lapply(paths, .video_quality_info)
    field <- function(y, key) {
        values <- y$value[y$key2 == key]
        if (length(values)) as.character(values[1]) else NA_character_
    }
    fps <- function(y) {
        value <- field(y, "video_avg_frame_rate")
        if (is.na(value) || value == "0/0") value <- field(y, "video_r_frame_rate")
        parts <- suppressWarnings(as.numeric(strsplit(value, "/", fixed = T)[[1]]))
        if (length(parts) == 2 && all(is.finite(parts)) && parts[2] > 0) {
            return(parts[1] / parts[2])
        }
        if (length(parts) == 1) return(parts)
        NA_real_
    }
    sizes <- lapply(info, function(y) {
        suppressWarnings(as.numeric(c(field(y, "video_width"), field(y, "video_height"))))
    })
    if (any(!is.finite(unlist(sizes)))) stop("both files must contain a video stream.")
    if (!identical(sizes$x, sizes$reference)) {
        stop("video resolutions differ; compare files with the same resolution.")
    }
    rates <- vapply(info, fps, numeric(1))
    if (all(is.finite(rates) & rates > 0)) {
        if (abs(rates[1] - rates[2]) > 0.001) {
            stop("video frame rates differ; matching frames are required for comparison.")
        }
    } else {
        warning("could not verify frame rates; ensure both inputs contain matching frames.")
    }
    for (i in c("video_pix_fmt", "video_color_range", "video_color_space",
                "video_color_transfer", "video_color_primaries")) {
        values <- vapply(info, field, character(1), key = i)
        known <- !is.na(values) & !values %in% c("unknown", "unspecified", "N/A")
        if (all(known) && values[1] != values[2]) {
            warning(i, " differs; pixel-format or color differences can affect scores.")
        }
    }
    transfers <- vapply(info, field, character(1), key = "video_color_transfer")
    if ("vmaf" %in% metrics && any(transfers %in% c("smpte2084", "arib-std-b67"))) {
        stop("the default VMAF model is for SDR; select SSIM/PSNR for these HDR inputs.")
    }
    durations <- vapply(info, function(y) {
        value <- suppressWarnings(as.numeric(field(y, "video_duration")))
        if (!is.finite(value)) value <- suppressWarnings(as.numeric(field(y, "format_duration")))
        value
    }, numeric(1)) - c(start, start_reference)
    if (any(is.finite(durations) & durations <= 0)) stop("start time is beyond the end of a video.")
    remaining <- if (is.null(duration)) durations else pmin(durations, duration)
    tolerance <- if (all(is.finite(rates) & rates > 0)) 1 / min(rates) else 0.1
    if (all(is.finite(remaining)) && abs(diff(remaining)) > tolerance + 0.001) {
        warning("remaining video durations differ; only the overlapping section will be scored.")
    }

    message("ffmpeg cmd: ", cmd)
    status <- system(paste(cmd, ">", shQuote(console_file), "2>&1"))
    log <- readLines(console_file, warn = F)
    if (status != 0) {
        stop("FFmpeg quality comparison failed (exit ", status, "):\n",
             paste(utils::tail(log, 20), collapse = "\n"))
    }
    frames <- lapply(metrics, function(metric) {
        path <- stats_files[[metric]]
        if (!file.exists(path) || file.info(path)$size == 0) {
            stop("FFmpeg produced no ", metric, " scores; check the selected time interval.")
        }
        if (metric == "vmaf") {
            df <- utils::read.csv(path, check.names = F)
            if (!all(c("Frame", "vmaf") %in% names(df))) stop("unrecognized VMAF log format.")
            frame <- df$Frame + 1L
            score <- df$vmaf
        } else {
            lines <- readLines(path, warn = F)
            frame <- .video_quality_log_value(lines, "n")
            score <- .video_quality_log_value(lines, if (metric == "ssim") "All" else "psnr_avg")
        }
        if (!length(score) || anyNA(score) || anyNA(frame)) {
            stop("FFmpeg produced missing or unreadable ", metric, " scores.")
        }
        tibble::tibble(metric = metric, frame = as.integer(frame), score = as.numeric(score))
    })
    summary <- lapply(frames, function(df) {
        metric <- df$metric[1]
        score <- if (metric == "vmaf") {
            mean(df$score)
        } else {
            lines <- log[grepl(if (metric == "ssim") "SSIM .*All:" else "PSNR .*average:", log)]
            value <- .video_quality_log_value(utils::tail(lines, 1), if (metric == "ssim") "All" else "average")
            if (length(value) != 1 || is.na(value)) stop("could not read overall ", metric, " score.")
            value
        }
        tibble::tibble(metric = metric,
                       unit = ifelse(metric == "psnr", "dB", "score"),
                       score = score,
                       min = min(df$score),
                       p05 = unname(stats::quantile(df$score, 0.05, type = 1)),
                       max = max(df$score),
                       frames = nrow(df))
    })
    files <- tibble::tibble(role = c("converted", "reference"),
                           file = c(x, reference),
                           size_megabyte = as.numeric(file.info(c(x, reference))$size) / 1e6)
    return(list(summary = dplyr::bind_rows(summary),
                frames = dplyr::bind_rows(frames),
                files = files,
                cmd = cmd,
                log = log))
}


.video_quality_log_value <- function(lines, key) {
    pattern <- paste0("(?:^|[[:space:]])", key, ":([^[:space:]]+)")
    matches <- regexec(pattern, lines, perl = T)
    values <- regmatches(lines, matches)
    vapply(values, function(y) {
        if (length(y) != 2) return(NA_real_)
        suppressWarnings(as.numeric(y[2]))
    }, numeric(1))
}


.video_quality_info <- function(x) {
    # Select exactly the video stream used by the comparison, even if more exist.
    entries <- paste0("stream=width,height,avg_frame_rate,r_frame_rate,pix_fmt,",
                      "color_range,color_space,color_transfer,color_primaries,duration:",
                      "format=duration")
    cmd <- paste(shQuote(unname(Sys.which("ffprobe"))),
                 "-v error -select_streams v:0 -show_entries", shQuote(entries),
                 "-of flat", shQuote(x), "2>&1")
    lines <- suppressWarnings(system(cmd, intern = T))
    status <- attr(lines, "status")
    if (!is.null(status) && status != 0) {
        stop("could not read video metadata for ", x, ":\n",
             paste(utils::tail(lines, 10), collapse = "\n"))
    }
    key <- sub("=.*$", "", lines)
    key <- sub("^streams\\.stream\\.0\\.", "video_", key)
    key <- sub("^format\\.", "format_", key)
    value <- sub("^[^=]*=", "", lines)
    value <- gsub('^"|"$', "", value)
    tibble::tibble(key2 = key, value = value)
}
