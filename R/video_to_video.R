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
#' @param overwrite overwrite files on disk? if FALSE and file exists in out_path,
#' file name is augmented by increasing integer at the end
#' @param start start time in seconds or format like 23:00 or 01:10:15;
#' -ss flag before -i (https://trac.ffmpeg.org/wiki/Seeking), define milliseconds
#' like so: 2.500 for 2 sec and 500 millis or 00:00:02.500
#' @param end end time, format like start; -to flag of ffmpeg
#' @param scale resize output images, c(width, height); either something like
#' c(1280, 720) for exact pixels or c(0.5, 0.5) for fraction of input
#' @param out_name name of output file w/o file extension
#' @param setpts setpts argument of -vf flag; controls playback speed of output file;
#' relative timestamp conversion of frames; if NULL, subject to change
#' depend upon fps_sample and fps_out
#' @param fps_out -f flag: fps in output file
#' @param audio_rm remove audio? -an flag
#' @param audio_encoder -c:a flag
#' @param audio_bitrate -b:a flag
#' @param encoder video encoder/format; checked in combination with
#' container
#' @param container output container/ file ext, if NULL same as input file ext
#' @param preset -preset flag: strength of compression
#' @param log_level amount of log info
#' @param metadata named vector of metadata keys to set
#' @param cmd_to_key save ffmpeg to any metadata key? provide the key name;
#' one of c("title", "artist", "composer", "album", "date", "comment",
#' "genre", "copyright", "track")
#' @param duration -t flag: number of seconds to encode from start; may be in
#' hh:mm:ss format of just a number of seconds
#' @param quality_crf -crf flag: see ?video_batch_convert
#' @param out_name_augment add flags to out_name to make it unique and/or
#' descriptive; TRUE, FALSE, "end" or "start" to place flags at end or start;
#' TRUE puts it at end
#' @param which_ffmpeg which version to use
#' @param run_cmd run ffmpeg command (TRUE) or just return the string (FALSE)
#' @param duration_frames how many frames to write from start
#' @param flags_add_before_i additional flags for ffmpeg command before input file
#' @param flags_add_after_i  additional flags for ffmpeg command after input file
#' @param ... for internal use
#' @param runtime_to_key write conversion time to a keyword?
#' @param skip_existing skip conversion of an existing output file on disk
#'
#' @returns cmd string
#' @export
#'
#' @examples
#' \dontrun{
#' # this makes timelapse correctly
#' # N = frame number starting at 0
#' # 30 = fps
#' # TB = time base (internal FFmpeg constant)
#' # https://stackoverflow.com/questions/41902160/create-time-lapse-video-from-other-video
#' video_to_video(
#'   x = x,
#'   flags_add = "-vf 'framestep=30,setpts=N/30/TB' -an",
#'   fps_out = 30,
#'   audio_rm = T
#' )
#' however this one does as well
#' setpts is inferred from fps_sample = 1 and fps_in = 30 (1/30)
#' however, -vf flag mus not contain "fps=1" but setpts only
#' but why does this cause frame dropping?
#' ffmpeg -y -v info -i 'in.mp4' -c:v libx264 -an -vf 'setpts=0.0333*PTS' -an 'out.mp4'
#' https://www.reddit.com/r/ffmpeg/comments/1bcfru6/speeding_up_a_video_by_5x_by_dropping_frames_and/
#' ffmpeg syntax remains difficult (or inconsistent?)
#' ffmpeg 4.4.6 and 7.1.1 behave different here! with v7, frames are not dropped
#' chatgpt also gives wrong answers
#' see https://ffmpeg.org/ffmpeg-filters.html#fps and https://trac.ffmpeg.org/wiki/ChangingFrameRate#Verifyingframeratechanges
#' so it is very complicated, but vf = fps=fps=25 as in av package appears there
#' }
video_to_video <- function(x,
                           out_path = NULL,
                           out_name = NULL,
                           fps_sample = NULL,
                           setpts = NULL,
                           fps_out = NULL,
                           audio_rm = F,
                           audio_encoder = NULL,
                           audio_bitrate = NULL,
                           encoder = c(
                               "",
                               "libx264",
                               "libx265",
                               "libvpx_vp9",
                               "gif",
                               "copy",
                               "h264_videotoolbox"
                           ),
                           container = c(
                               "",
                               "mp4",
                               "mkv",
                               "webm",
                               "gif",
                               "avi",
                               "mov"
                           ),
                           preset = c(
                               "",
                               "medium",
                               "ultrafast",
                               "superfast",
                               "veryfast",
                               "faster",
                               "fast",
                               "slow",
                               "slower",
                               "veryslow"
                           ),
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
                           metadata = c(title = NULL,
                                        artist = NULL,
                                        composer = NULL,
                                        album = NULL,
                                        date = NULL,
                                        comment = NULL,
                                        genre = NULL,
                                        copyright = NULL,
                                        track = NULL),
                           cmd_to_key = c(
                               "",
                               "title",
                               "artist",
                               "composer",
                               "album",
                               "date",
                               "comment",
                               "genre",
                               "copyright",
                               "track"),
                           runtime_to_key = c(
                               "",
                               "title",
                               "artist",
                               "composer",
                               "album",
                               "date",
                               "comment",
                               "genre",
                               "copyright",
                               "track"),
                           overwrite = F,
                           start = NULL,
                           end = NULL,
                           duration = NULL,
                           duration_frames = NULL,
                           scale = NULL,
                           quality_crf = NULL,
                           out_name_augment = F,
                           flags_add_before_i = "",
                           flags_add_after_i = "",
                           which_ffmpeg = c("ffmpeg", "ffmpeg7"),
                           run_cmd = T,
                           skip_existing = F,
                           ...) {

    if (!requireNamespace("brathering", quietly = T)) {
        devtools::install_github("Close-your-eyes/brathering")
    }


    # find ffmpeg more general:
    paths <- system("echo $PATH | tr ':' '\n'", intern = T)
    allbins <- unique(unlist(purrr::map(paths, list.files)))
    ffmpegs <- grep("ffmpeg", allbins, value = T)
    if (!length(ffmpegs)) {
            message("FFmpeg not installed or not in PATH.")
            message("Install with macports: sudo port install ffmpeg")
            return(NULL)
    }


    dots <- list(...)


    # if (!nzchar(Sys.which("ffmpeg")) && !nzchar(Sys.which("ffmpeg7"))) {
    #     message("FFmpeg nor FFmpeg7 is installed or not in PATH.")
    #     message("Install with macports: sudo port install ffmpeg")
    #     return(NULL)
    # } else if (nzchar(Sys.which("ffmpeg7"))) {
    #     ff <- "ffmpeg7"
    # } else if (nzchar(Sys.which("ffmpeg"))) {
    #     ff <- "ffmpeg"
    # }

    ff <- rlang::arg_match(which_ffmpeg, values = ffmpegs)
    # if (nzchar(Sys.which(which_ffmpeg))) {
    #     ff <- which_ffmpeg
    # }

    if (sum(!is.null(end), !is.null(duration), !is.null(duration_frames)) > 1) {
        stop("either set end or duration or duration_frames.")
    }

    x <- unshquote(x)
    infile_flag <- ""
    ## special case for img_to_video
    if (is.null(dots[["skip_x_check"]])) {
        if (!file.exists(x)) {
            stop(x, " not found.")
        }
        if (fs::is_dir(x)) {
            stop("x must be a file, not a directory.")
        }
        #info <- av::av_media_info(x)
        info <- media_info(x)
        infile_flag <- glue::glue("-i {shQuote(x)}")
    }

    out_path <- ifelse(is.null(out_path), file.path(dirname(x)), suppressWarnings(normalizePath(out_path)))
    dir.create(out_path, recursive = T, showWarnings = F)

    cmd_to_key <- rlang::arg_match(cmd_to_key)
    runtime_to_key <- rlang::arg_match(runtime_to_key)
    preset <- rlang::arg_match(preset)
    encoder <- rlang::arg_match(encoder)
    log_level <- rlang::arg_match(log_level)
    container <- rlang::arg_match(container)


    if (runtime_to_key != "" && runtime_to_key == cmd_to_key) {
        stop("runtime_to_key and cmd_to_key should be unequal.")
    }

    totalsec <- info |>
        dplyr::filter(key2 == "format_duration") |>
        dplyr::mutate(value = as.numeric(value)) |>
        dplyr::pull(value)
    fps_in <- info |>
        dplyr::filter(key2 == "video_r_frame_rate") |>
        #dplyr::mutate(value = as.numeric(strsplit(value, "/")[[1]][1])) |>
        #dplyr::rowwise() |>
        dplyr::mutate(value = eval(parse(text = value))) |>
        dplyr::pull(value)
    video_width <- info |>
        dplyr::filter(key2 == "video_width") |>
        dplyr::mutate(value = as.numeric(value)) |>
        dplyr::pull(value)
    video_height <- info |>
        dplyr::filter(key2 == "video_height") |>
        dplyr::mutate(value = as.numeric(value)) |>
        dplyr::pull(value)
    video_codec <- info |>
        dplyr::filter(key2 == "video_codec_name") |>
        dplyr::pull(value)

    # do alter setpts below?
    alt_pts <- F
    if (is.null(setpts)) {
        setpts <- 1
        alt_pts <- T
    }


    # set r_flag here to allow change of fps_out below, for setting setpts
    r_flag <- ifelse(is.null(fps_out), "", glue::glue("-r {fps_out}"))
    fps_out <- fps_out %||% fps_in
    if (alt_pts) {
        setpts <- setpts*fps_in/fps_out # if 30/60 --> setpts*0.5 meaning 2x playback
    }
    # fps -vf flag
    # auto set filterv_flag with setpts based on fps_sample and fps_out
    # PTS is original timestamp of frames
    # setpts=0.5*PTS would reduce each timestamp by 50%, resulting in 2x faster playback and duration/2

    vf_fps_flag <- ifelse(is.null(fps_sample), "", glue::glue("fps={fps_sample}")) # do before fps_sample could become fps_in
    if (is.null(fps_sample)) {
        fps_sample <- fps_in # not needed here but for video_to_img
    }
    if (alt_pts) {
        setpts <- setpts*fps_sample/fps_in # e.g 1/30 --> 30x faster playback
    }

    vf_setpts_flag <- ifelse(dplyr::near(setpts, 1), "", glue::glue("setpts={setpts}*PTS"))
    vf_scale_flag <- ifelse(is.null(scale), "", get_vf_scale_flag(scale = scale, video_width = video_width, video_height = video_height))
    vf_arg <- paste(c(vf_scale_flag, vf_setpts_flag), collapse = ",")
    vf_arg <- sub("^,", "", vf_arg)
    vf_flag <- paste0("-vf ", shQuote(vf_arg)) # vf_fps_flag # fps=fps=25 ? ffmpeg7
    vf_flag <- ifelse(vf_flag %in% c("-vf ','", "-vf ',,'", "-vf ''"), "", vf_flag) # %in% two options in case vf_fps_flag is re-included

    if (container == "") {
        container <- tools::file_ext(basename(x))
    }
    container <- select_output_format(input_container = tools::file_ext(basename(x)),
                                      input_codec = video_codec,
                                      output_container = tolower(container),
                                      codec = encoder)

    cv_flag <- ifelse(container == "gif" || encoder == "", "", glue::glue("-c:v {encoder}"))
    preset_flag <- ifelse(preset == "", "", glue::glue("-preset {preset}"))
    ss_flag <- ifelse(is.null(start), "", glue::glue("-ss {start}"))
    to_flag <- ifelse(is.null(end), "", glue::glue("-to {end}"))
    t_flag <- ifelse(is.null(duration), "", glue::glue("-t {duration}"))
    fv_flag <- ifelse(is.null(duration_frames), "", glue::glue("-frames:v {duration_frames}"))
    an_flag <- ifelse(audio_rm, "-an", "")
    ca_flag <- ifelse(is.null(audio_encoder) || audio_rm, "", ca_flag <- glue::glue("-c:a {audio_encoder}"))
    ba_flag <- ifelse(is.null(audio_bitrate) || audio_rm, "", glue::glue("-b:a {audio_bitrate}"))
    crf_flag <- ifelse(is.null(quality_crf), "", glue::glue("-crf {quality_crf}"))
    log_flag <- ifelse(log_level == "", "", glue::glue("-v {log_level}"))


    out_name <- ifelse(is.null(out_name), tools::file_path_sans_ext(basename(x)), basename(out_name))


    if (!identical(out_name_augment, FALSE)) {
        append <- glue::glue("{ff}__{ss_flag}__{to_flag}__{t_flag}__{fv_flag}__{cv_flag}__{r_flag}__{an_flag}__{preset_flag}__{crf_flag}__{vf_flag}__{ca_flag}__{ba_flag}")
        append <- gsub("_{3,}", "__", append)
        append <- gsub("[='*-]", "", append)
        append <- gsub(":", ".", append)
        append <- gsub("[ ,]", "_", append)
        append <- gsub("_{1,}$", "", append)
        if (identical(out_name_augment, TRUE) || identical(out_name_augment, "end")) {
            out_name <- glue::glue("{out_name}___{append}")
        } else {
            out_name <- glue::glue("{append}___{out_name}")
        }
    }

    container <- paste0(".", container)
    output <- file.path(out_path, ifelse(grepl(paste0("\\",container,"$"), out_name), out_name, paste0(out_name, container)))
    if (!overwrite && !skip_existing) {
        output <- brathering::make_filepath_unique(output)
    }
    outfile_flag <- glue::glue("{shQuote(output)}")

    overwrite_flag <- ifelse(overwrite, "-y", "-n") # due to handling above this becomes irrelevant; -y always works now

    if (skip_existing && file.exists(output)) {
        run_cmd <- F
        message("File ", output, " exists. Skipping cmd execution.")
    }
    metaflag <- ""

    cmd <- make_cmd_and_metaflag(cmd_glue_str = "{ff} {flags_add_before_i} {overwrite_flag} {log_flag} {ss_flag} {to_flag} {infile_flag} {t_flag} {fv_flag} {cv_flag} {r_flag} {an_flag} {preset_flag} {crf_flag} {ca_flag} {ba_flag} {flags_add_after_i} {metaflag} {vf_flag} {outfile_flag}",
                                 ff = ff,
                                 overwrite_flag = overwrite_flag,
                                 log_flag = log_flag,
                                 ss_flag = ss_flag,
                                 infile_flag = infile_flag,
                                 t_flag = t_flag,
                                 fv_flag = fv_flag,
                                 to_flag = to_flag,
                                 cv_flag = cv_flag,
                                 r_flag = r_flag,
                                 an_flag = an_flag,
                                 preset_flag = preset_flag,
                                 crf_flag = crf_flag,
                                 flags_add_before_i = flags_add_before_i,
                                 flags_add_after_i = flags_add_after_i,
                                 ca_flag = ca_flag,
                                 ba_flag = ba_flag,
                                 metaflag = metaflag,
                                 vf_flag = vf_flag,
                                 outfile_flag = outfile_flag,
                                 metadata = metadata,
                                 cmd_to_key = cmd_to_key)

    if (run_cmd) {
        message("ffmpeg cmd: ", cmd)

        if (runtime_to_key != "") {
            msg <- system.time(system(cmd, intern = T))
            runtime <- stats::setNames(as.numeric(msg), names(msg))
            runtime <- runtime[["elapsed"]]-runtime[["sys.self"]]-runtime[["sys.child"]]
            output2 <- brathering::make_filepath_unique(output)
            outfile_flag2 <- glue::glue("{shQuote(output2)}")
            cmd2 <- glue::glue("{ff} -y -i {outfile_flag} -metadata {runtime_to_key}={runtime} -c copy {outfile_flag2}")
            msg2 <- system(cmd2, intern = T)
            fs::file_delete(output)
            fs::file_move(output2, output)
        } else {
            system(cmd, intern = T)
        }

    }

    return(cmd)
}


make_cmd_and_metaflag <- function(cmd_glue_str,
                                  ff,
                                  overwrite_flag,
                                  log_flag,
                                  ss_flag,
                                  infile_flag,
                                  t_flag,
                                  to_flag,
                                  fv_flag,
                                  cv_flag,
                                  r_flag,
                                  an_flag,
                                  preset_flag,
                                  crf_flag,
                                  ca_flag,
                                  ba_flag,
                                  flags_add_before_i,
                                  flags_add_after_i,
                                  metaflag,
                                  vf_flag,
                                  outfile_flag,
                                  metadata,
                                  cmd_to_key) {

    # position of  {filterv_flag} before output is important!!!
    cmd <- stringr::str_squish(glue::glue(cmd_glue_str))
    # cmd in metadata w/o metaflag
    if (cmd_to_key != "") {
        if (cmd_to_key %in% names(metadata)) {
            metadata[[cmd_to_key]] <- cmd
        } else {
            metadata <- c(metadata, stats::setNames(cmd, cmd_to_key))
        }
    }
    if (!is.null(metadata)) {
        for (i in seq_along(metadata)) {
            metaflag <- paste0(metaflag, " -metadata ", names(metadata[i]), "=", shQuote(metadata[i]))
        }
    }
    # change if metadata is added
    cmd <- stringr::str_squish(glue::glue(cmd_glue_str))
    return(cmd)
}


select_output_format <- function(input_container, input_codec, codec = NULL, output_container = NULL) {

    # Define supported combinations for re-encoding
    codec_map <- list(
        libx264      = c("mp4", "mkv", "mov", "flv", "ts", "avi"),
        libx265      = c("mp4", "mkv", "mov", "ts"),
        libvpx_vp9   = c("webm"),
        h264_videotoolbox = c("mp4"),
        gif          = c("gif") # special case
    )

    # Define common container support for raw codecs (for c:v copy)
    container_support <- list(
        mp4  = c("h264", "hevc", "avc1"),
        mkv  = c("h264", "hevc", "vp9", "av1"),
        mov  = c("h264", "hevc"),
        flv  = c("h264"),
        ts   = c("h264", "hevc"),
        avi  = c("mpeg4", "h264"),
        webm = c("vp8", "vp9")
    )

    if (codec == "") {
        # -c:v flag remains empty and ffmpeg decides encder based on output_container
        return(output_container)
    }
    # If no codec specified, assume copy
    if (is.null(codec)) {
        codec <- "copy"
    }

    # If no output container provided, default to input container
    if (is.null(output_container)) {
        output_container <- input_container
    }

    # Logic for codec = copy
    if (codec == "copy") {
        # Check if output container supports the input codec
        if (output_container %in% names(container_support) && input_codec %in% container_support[[output_container]]) {
            return(output_container)  # Valid combination
        } else {
            # Find a container that supports the input codec
            valid_container <- names(container_support)[sapply(container_support, function(x) input_codec %in% x)][1]
            if (!is.na(valid_container)) {
                message(sprintf("'%s' does not support codec '%s'. Using '%s' instead.",
                                output_container, input_codec, valid_container))
                return(valid_container)
            } else {
                stop(sprintf("No suitable container found for codec '%s' when copying.", input_codec))
            }
        }
    }

    # Logic for re-encode (non-copy)
    if (!codec %in% names(codec_map)) {
        stop(sprintf("Unsupported codec: %s", codec))
    }

    # Validate output container for chosen codec
    if (!(output_container %in% codec_map[[codec]])) {
        # If invalid, fallback to first valid container
        message(sprintf("Container '%s' is not valid for codec '%s'. Using '%s' instead.",
                        output_container, codec, codec_map[[codec]][1]))
        output_container <- codec_map[[codec]][1]
    }

    return(output_container)
}

get_vf_scale_flag <- function(scale, video_width, video_height) {
    if (length(scale) != 2) {
        stop("scale must be numeric of length 2.")
    }
    if (sum(is.na(scale) == 2)) {
        stop("scale: both cannot be NA.")
    }

    # scale=iw/2:ih/2 also possible with ffmpeg iw = input width
    scale[1] <- ifelse(scale[1]<1, video_width*scale[1], scale[1])
    scale[2] <- ifelse(scale[2]<1, video_height*scale[2], scale[2])

    if (is.na(scale[1])) {
        scale[1] <- video_width*scale[2]/video_height
    }
    if (is.na(scale[2])) {
        scale[2] <- video_height*scale[1]/video_width
    }

    # codec requires even values
    for (i in 1:2) {
        if (scale[i] %% 2 != 0) {
            scale[i] <- scale[i]-1
        }
    }
    scale <- paste0(scale[1], ":", scale[2])
    vf_scale_flag <- glue::glue("scale={scale}")
    return(vf_scale_flag)
}
