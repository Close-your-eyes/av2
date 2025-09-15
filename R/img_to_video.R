#' Create video from image sequence
#'
#' Images need to be in one folder and named in sequential order
#'
#' @param x path to image folder
#' @param out_path output folder path; if NULL: folder created next to x with
#' filename of x
#' @param out_name name of output file w/o file extension
#' @param overwrite overwrite files on disk? if FALSE and file exists in out_path,
#' file name is augmented by increasing integer at the end
#' @param fps_out -f flag: fps in output file
#' @param scale resize output images, c(width, height); either something like
#' c(1280, 720) for exact pixels or c(0.5, 0.5) for fraction of input
#' @param encoder video encoder/format; checked in combination with
#' container
#' @param container output container/ file ext, if NULL same as input file ext
#' @param log_level amount of log info
#' @param quality_crf -crf flag: see ?video_batch_convert
#' @param run_cmd run ffmpeg command (TRUE) or just return the string (FALSE)
#' @param flags_add_before_i additional flags for ffmpeg command before input file
#' @param flags_add_after_i  additional flags for ffmpeg command after input file
#' @param which_ffmpeg which version to use
#'
#' @returns
#' @export
#'
#' @examples
img_to_video <- function(x,
                         out_path = NULL,
                         out_name = "out",
                         overwrite = F,
                         fps_out = 30,
                         scale = NULL,
                         encoder = c(
                             "libx264",
                             "libx265",
                             "libvpx_vp9",
                             "gif"),
                         container = c(
                             "mp4",
                             "mkv",
                             "webm",
                             "gif",
                             "avi",
                             "mov"),
                         log_level = c(
                             "",
                             "info",    # Default level
                             "quiet",   # No output except fatal errors
                             "panic",   # Only panic messages
                             "fatal",   # Only fatal errors
                             "error",   # Only errors
                             "warning", # Warnings and errors
                             "verbose", # More detailed info
                             "debug",   # Debugging info
                             "trace"    # Extremely detailed logs
                         ),
                         quality_crf = NULL,
                         run_cmd = T,
                         flags_add_before_i = "",
                         flags_add_after_i = "-pix_fmt yuv420p",
                         which_ffmpeg = c("ffmpeg", "ffmpeg7")) {

    encoder <- rlang::arg_match(encoder)
    container <- rlang::arg_match(container)

    imgfiles <- list.files(x)
    ext <- tools::file_ext(imgfiles)
    ext <- names(which.max(table(ext)))

    infile <- shQuote(paste0(x, "/*.", ext))

    flags_add_before_i <- paste0(trimws(flags_add_before_i), "-pattern_type glob -i ", infile, " -framerate ", fps_out)

    # prep info for inject via ...
    imgfilesfull <- list.files(x, full.names = T, pattern = paste0(ext, "$"))
    imginfo <- magick::image_info(image = magick::image_read(imgfilesfull[1]))
    info <- list(duration = 1)
    info[["video"]][["width"]] <- imginfo$width
    info[["video"]][["height"]] <- imginfo$height
    info[["video"]][["framerate"]] <- fps_out

    out_path <- ifelse(is.null(out_path), x, out_path)

    arglist <- list(x = x,
                    out_path = out_path,
                    overwrite = overwrite,
                    log_level = log_level,
                    out_name = out_name,
                    encoder = encoder,
                    container = container,
                    scale = scale,
                    fps_out = fps_out,
                    which_ffmpeg = which_ffmpeg,
                    flags_add_before_i = flags_add_before_i,
                    flags_add_after_i = flags_add_after_i,
                    quality_crf = quality_crf,
                    run_cmd = run_cmd,
                    # deactivated flags:
                    start = NULL,
                    end = NULL,
                    duration_frames = NULL,
                    fps_sample = NULL,
                    audio_rm = F,
                    setpts = 1,
                    audio_encoder = NULL,
                    audio_bitrate = NULL,
                    preset = "",
                    metadata = NULL,
                    cmd_to_key = "",
                    duration = NULL,
                    out_name_augment = F,
                    # injects via ...
                    skip_x_check = T,
                    info = info)


    do.call(video_to_video, args = arglist)

}
