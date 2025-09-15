#' Create image sequence from video
#'
#' Calling ffmpeg with system() was found much faster than av::av_encode_video.
#' Install ffmpeg or ffmpeg7 with macports: sudo port install ffmpeg or
#' sudo port install ffmpeg7. Non of them was found superior, yet.
#' This is a wrapper to create a ffmpeg call simplifying it but not providing
#' full complexity.
#'
#' @param x path to video file on disk
#' @param out_path output folder path; if NULL: folder created next to x with
#' filename of x
#' @param fps_sample how many frames per second to sample from input;
#' check media info with av::av_media_info(x), NULL to sample all
#' @param total_frames alternative to fps_sample, see total_frames_evenly
#' for interpretation
#' @param total_frames_evenly total frames sampled evenly across duration of
#' x of total images sampled from start
#' @param format output image format; all jpeg formats yield .jpg file
#' extension; see av::av_encoders(), options here were found working with
#' calling ffmpeg directly
#' @param out_prefix image name prefix; if NULL filename of x
#' @param out_suffix must be of format %id with i being any number to define
#' number of leading zeros; NULL for automatic setting based on total
#' expected images
#' @param flags_add_before_i additional flags for ffmpeg command before input, e.g. for
#' jpg: -q:v 2 (lower is better (range: 2–31)) or for png:
#' -compression_level 0 (0 (fast) to 100 (max compression));
#' @param overwrite overwrite files on disk?
#' @param start start time in seconds or format like 23:00 or 01:10:15;
#' -ss flag before -i (https://trac.ffmpeg.org/wiki/Seeking)
#' @param end end time, format like start; -to flag of ffmpeg
#' @param scale resize output images, c(width, height); either something like
#' c(1280, 720) for exact pixels or c(0.5, 0.5) for fraction of input
#' @param which_ffmpeg ffmpeg version
#' @param flags_add_after_i additional flags for ffmpeg command after input
#' @param log_level amount of log info printed
#'
#' @returns nothing
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "/Users/chris/Downloads/001_GOPR0006.MP4"
#' video_to_img(x = x, format = "png", total_frames = 10, total_frames_evenly = T)
#' }
video_to_img <- function(x,
                         out_path = NULL,
                         fps_sample = NULL,
                         total_frames = NULL,
                         total_frames_evenly = T,
                         format = c("png", "jpeg2000", "jpegls", "mjpeg", "tiff", "bmp"),
                         out_prefix = NULL,
                         out_suffix = NULL,
                         overwrite = T,
                         start = NULL,
                         end = NULL,
                         scale = NULL,
                         which_ffmpeg = c("ffmpeg", "ffmpeg7"),
                         flags_add_before_i = "",
                         flags_add_after_i = "",
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
                         )) {

    if (!is.null(total_frames) && !is.null(fps_sample)) {
        stop("set total_frames or total_frames, not both.")
    }

    format <- rlang::arg_match(format)
    info <- av::av_media_info(x)
    totalsec <- info$duration
    fps_in <- info$video$framerate


    # fps -vf flag
    frames_flag <- NULL
    if (!is.null(total_frames)) {
        if (total_frames_evenly) {
            # this forces -vf flag below
            fps_sample <- total_frames/totalsec
        } else {
            frames_flag <- glue::glue("-frames:v {total_frames}")
        }
    }

    if (is.null(fps_sample)) {
        fps_sample <- fps_in
        vf_flag <- ""
    } else {
        vf_flag <- glue::glue("-vf fps={fps_sample}")
    }
    frames_total <- ifelse(!is.null(total_frames) && !total_frames_evenly, total_frames, floor(totalsec*fps_sample))
    message(frames_total, " images will be written to disk")

    # pass vf_flag via flags_add
    flags_add_after_i <- paste0(trimws(flags_add_after_i), " ", vf_flag, " -c:v ", format)

    ## output flag
    out_ext <- ifelse(grepl("jpeg", format), "jpg", format)
    out_prefix <- out_prefix %||% paste0(basename(tools::file_path_sans_ext(x)), "_")
    out_suffix <- out_suffix %||% paste0("%", nchar(frames_total), "d")
    if (!(grepl("^%[[:digit:]]{1,}d$", out_suffix) &&
          as.numeric(stringr::str_extract(out_suffix, "[[:digit:]]{1,}")) >= nchar(frames_total))) {
        stop("out_suffix has to have format %[[:digit:]]{1,}d and digit needs to be >= ", nchar(frames_total))
    }
    out_name <- paste0(out_prefix, out_suffix, '.', out_ext)

    # make fps_sample in flags_add
    arglist <- list(x = x,
                    out_path = out_path,
                    overwrite = overwrite,
                    log_level = log_level,
                    out_name = out_name,
                    scale = NULL,
                    which_ffmpeg = which_ffmpeg,
                    run_cmd = F,
                    start = start,
                    end = end,
                    duration_frames = frames_flag,
                    flags_add_before_i = flags_add_before_i,
                    flags_add_after_i = flags_add_after_i,
                    # deactivated flags:
                    fps_sample = NULL,
                    audio_rm = T,
                    setpts = 1,
                    fps_out = NULL,
                    audio_encoder = NULL,
                    audio_bitrate = NULL,
                    encoder = "",
                    container = "",
                    preset = "",
                    metadata = NULL,
                    cmd_to_key = "",
                    duration = NULL,
                    quality_crf = NULL,
                    out_name_augment = F)


    cmd <- do.call(video_to_video, args = arglist)
    # remove container ext
    cmd <- stringi::stri_reverse(sub(paste0(stringi::stri_reverse(tools::file_ext(x)), "\\."), "", stringi::stri_reverse(cmd)))
    system(cmd, intern = T)
}





#' Create image sequence from video
#'
#' Calling ffmpeg with system() was found much faster than av::av_encode_video.
#' Install ffmpeg or ffmpeg7 with macports: sudo port install ffmpeg or
#' sudo port install ffmpeg7. Non of them was found superior, yet.
#' This is a wrapper to create a ffmpeg simplifying the call but not providing
#' full complexity.
#'
#' @param x path to video file on disk
#' @param out_path output folder path; if NULL: folder created next to x with
#' filename of x
#' @param fps_sample how many frames per second to sample from input;
#' check media info with av::av_media_info(x), NULL to sample all
#' @param total_frames alternative to fps_sample, see total_frames_evenly
#' for interpretation
#' @param total_frames_evenly total frames sampled evenly across duration of
#' x of total images sampled from start
#' @param img_format output image format; all jpeg formats yield .jpg file
#' extension; see av::av_encoders(), options here were found working with
#' calling ffmpeg directly
#' @param img_prefix image name prefix; if NULL filename of x
#' @param img_suffix must be of format %id with i being any number to define
#' number of leading zeros; NULL for automatic setting based on total
#' expected images
#' @param ffmpeg_flags additional flags for ffmpeg command, e.g. for
#' jpg: -q:v 2 (lower is better (range: 2–31)) or for png:
#' -compression_level 0 (0 (fast) to 100 (max compression));
#' -hwacell opencl or videotoolbox did not have effect because
#' VideoToolbox does not currently accelerate decoding-only tasks in FFmpeg on macOS
#' @param overwrite overwrite files on disk?
#' @param start start time in seconds or format like 23:00 or 01:10:15;
#' -ss flag before -i (https://trac.ffmpeg.org/wiki/Seeking)
#' @param end end time, format like start; -to flag of ffmpeg
#' @param scale resize output images, c(width, height); either something like
#' c(1280, 720) for exact pixels or c(0.5, 0.5) for fraction of input
#'
#' @returns nothing
#'
#' @examples
#' \dontrun{
#' x <- "/Users/chris/Downloads/001_GOPR0006.MP4"
#' video_to_img(x = x, format = "png", total_frames = 10, total_frames_evenly = T)
#' }
video_to_img_old <- function(x,
                             out_path = NULL,
                             fps_sample = NULL,
                             total_frames = NULL,
                             total_frames_evenly = T,
                             img_format = c("png", "jpeg2000", "jpegls", "mjpeg", "tiff", "bmp"),
                             img_prefix = NULL,
                             img_suffix = NULL,
                             overwrite = T,
                             start = NULL,
                             end = NULL,
                             scale = NULL,
                             ffmpeg_flags = "") {

    if (!nzchar(Sys.which("ffmpeg")) && !nzchar(Sys.which("ffmpeg7"))) {
        message("FFmpeg nor FFmpeg7 is installed or not in PATH.")
        message("Install with macports: sudo port install ffmpeg")
        return(NULL)
    } else if (nzchar(Sys.which("ffmpeg"))) {
        ff <- "ffmpeg"
    } else if (nzchar(Sys.which("ffmpeg7"))) {
        ff <- "ffmpeg7"
    }

    if (!file.exists(x)) {
        message(x, " not found.")
    }
    if (is.null(out_path)) {
        out_path <- file.path(dirname(x), basename(tools::file_path_sans_ext(x)))
        message("out_path: ", out_path)
    }
    dir.create(out_path, recursive = T, showWarnings = F)

    img_format <- rlang::arg_match(img_format)
    info <- av::av_media_info(x)
    totalsec <- info$duration
    fps_in <- info$video$framerate

    # fps -vf flag
    if (!is.null(total_frames)) {
        if (total_frames_evenly) {
            fps_sample <- total_frames/totalsec
            frames_flag <- ""
        } else {
            frames_flag <- glue::glue("-frames:v {total_frames}")
        }
    }

    if (is.null(fps_sample)) {
        fps_sample <- fps_in
        vf_flag <- "-vf"
    } else {
        vf_flag <- glue::glue("-vf fps={fps_sample}")
    }
    if (!is.null(total_frames) && !total_frames_evenly) {
        frames_total <- total_frames
    } else {
        frames_total <- ceiling(totalsec*fps_sample)
    }
    message(frames_total, " images will be written to disk")



    ## output flag
    img_ext <- ifelse(grepl("jpeg", img_format), "jpg", img_format)
    img_prefix <- img_prefix %||% paste0(basename(tools::file_path_sans_ext(x)), "_")
    img_suffix <- img_suffix %||% paste0("%", nchar(frames_total), "d")
    if (!(grepl("^%[[:digit:]]{1,}d$", img_suffix) &&
          as.numeric(stringr::str_extract(img_suffix, "[[:digit:]]{1,}")) >= nchar(frames_total))) {
        stop("img_suffix has to have format %[[:digit:]]{1,}d and digit needs to be >= ", nchar(frames_total))
    }
    output <- file.path(out_path, paste0(img_prefix, img_suffix, '.', img_ext))

    overwrite <- ifelse(overwrite, "-y", "-n")

    if (!is.null(start)) {
        if (!grepl(":", start)) {
            # start in plain seconds
            p <- lubridate::seconds_to_period(as.numeric(start))
            start <- sprintf("%02d:%02d:%02d", lubridate::hour(p), lubridate::minute(p), lubridate::second(p))
        }
        ss_flag <- glue::glue("-ss {start}")
    } else {
        ss_flag <- ""
    }

    if (!is.null(end)) {
        if (!grepl(":", end)) {
            # start in plain seconds
            p <- lubridate::seconds_to_period(as.numeric(end))
            end <- sprintf("%02d:%02d:%02d", lubridate::hour(p), lubridate::minute(p), lubridate::second(p))
        }
        to_flag <- glue::glue("-to {end}")
    } else {
        to_flag <- ""
    }

    if (!is.null(scale)) {
        if (length(scale) != 2) {
            stop("scale must be numeric of length 2.")
        }
        # scale=iw/2:ih/2 also possible with ffmpeg iw = input width
        scale[1] <- ifelse(scale[1]<1, info$video$width*scale[1], scale[1])
        scale[2] <- ifelse(scale[2]<1, info$video$height*scale[2], scale[2])
        scale <- paste0(scale[1], ":", scale[2])

        vf_flag <- glue::glue("{vf_flag},scale={scale}")
        vf_flag <- paste0("-vf ", shQuote(gsub("-vf ", "", vf_flag)))
    }

    if (vf_flag == "-vf") {
        vf_flag <- ""
    }
    # slower
    # av::av_encode_video(input = x,
    #                     output = file.path(out_path, paste0(img_prefix, img_suffix, ".", img_ext)),
    #                     framerate = fps_in,
    #                     codec = img_format,
    #                     vfilter = paste0("fps=fps=", fps_sample))


    # -vsync 0 -hwaccel videotoolbox -preset ultrafast -f image2 -pix_fmt yuv420p", no effect
    # ffmpeg vs ffmpeg7 no effect
    # framerate = fps_in, not used
    # shQuote: save to send paths to shell in case spaces or special chars are contained
    # -an: no audio
    cmd <- glue::glue("{ff} -an {ffmpeg_flags} {overwrite} {ss_flag} -i {shQuote(x)} {to_flag} -c:v {img_format} {vf_flag} {frames_flag} {shQuote(output)}")
    cmd <- stringr::str_squish(cmd)

    message("call: ", cmd)
    system(cmd, intern = T)
}


