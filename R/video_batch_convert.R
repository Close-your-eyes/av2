#' Batch process and convert multiple video files to a common output format
#'
#' Originally intended to batch convert youtube video files. Rather made
#' to reduce (harmonize) fps, width and/or height.
#'
#' @param x dir with video files only, or vector of paths to video files
#' @param make_portable make filenames portable with brathering::make_portable_filename;
#' this will rename the originals
#' @param cut_codec cut names at codec info, everything behind is discarded;
#' this will rename the originals
#' @param codec_regexpr regular expression where to cut filenames
#' @param video_to_video_args arguments to video_to_video
#' @param fps_reduce_to integer fps to reduce to; adds fps_sample and
#' fps_out to video_to_video_args; video with lower fps are not altered
#' @param width_reduce_to integer video width to reduce to; adds scale argument
#' to video_to_video_args; lower video widths remain; if height_reduce_to is
#' NULL, heights are scaled proportionally to maintain aspect ratio
#' @param height_reduce_to analogue to width_reduce_to
#' @param hashtag_rm remove hashtags from filenames? all letters except spaces
#' after a # are removed
#'
#' @returns nothing
#' @export
#'
#' @examples
#' \dontrun{
#' x <- "~/Downloads/yt_vid"
#' # convert all by common codec but keep fps and resolution
#' video_batch_convert(x = x)
#' # just remove audio
#' video_batch_convert(x = x, video_to_video_args = list(audio_rm = T))
#' # reduce fps and width
#' video_batch_convert(x = x, fps_reduce_to = 10, width_reduce_to = 400)
#' # reduce quality and harmonize audio, also use other fmpeg version
#' video_batch_convert(x = x, video_to_video_args = list(out_path = "~/Downloads/yt_vid3",
#'                                                       ffmpeg_flags = "-crf 28 -c:a aac -b:a 128k",
#'                                                       which_ffmpeg = "ffmpeg7"))
#' # Example (for 1080p30 H.264):
#' # CRF 18: ~15–20 Mbps (visually lossless)
#' # CRF 23: ~8–10 Mbps
#' # CRF 28: ~4 Mbps
#' # test conversion with first 10 sec only
#' video_batch_convert(x = x,
#'                     fps_reduce_to = 25,
#'                     width_reduce_to = 576,
#'                     video_to_video_args = list(out_path = "~/Downloads/yt_vid2",
#'                                                ffmpeg_flags = "-crf 28 -c:a aac -b:a 128k -t 10",
#'                                                which_ffmpeg = "ffmpeg7"))
#' # rough CRF matches
#' #     | libx264 CRF | ≈ libx265 CRF |
#' #     | ----------- | ------------- |
#' #     | 18          | 20–21         |
#' #     | 20          | 22–23         |
#' #     | 23          | 26–28         |
#' #     | 25          | 28–30         |
#'
#' # change folder names first
#' vids <- list.files("/Users/chris/Downloads/yt", recursive = T, full.names = T)
#' new_paths <- gsub("\\S*#\\S*\\s?", "", dirname(vids)) # rm hashtags
#' new_paths <- make_portable_filepath(new_paths)
#' file.rename(dirname(vids), new_paths)
#'
#'}
video_batch_convert <- function(x,
                                make_portable = T,
                                cut_codec = T,
                                codec_regexpr = "[:digit:]{1,}p_",
                                hashtag_rm = T,
                                fps_reduce_to = NULL,
                                width_reduce_to = NULL,
                                height_reduce_to = NULL,
                                video_to_video_args = list()) {

    if (!requireNamespace("brathering", quietly = T)) {
        devtools::install_github("Close-your-eyes/brathering")
    }

    #handle filenames etc first
    x <- x_dir_or_files(x)

    # if ((make_portable || cut_codec || hashtag_rm) && is.null(out_path)) {
    #     stop("out_path needed when make_portable || cut_codec || hashtag_rm")
    # }
    #
    # if (!is.null(out_path)) {
    #     dir.create(out_path, recursive = T, showWarnings = F)
    #     fs::file_copy(x, file.path(out_path, basename(x)))
    #     x <- file.path(out_path, basename(x))
    # }

    xdir <- dirname(x)
    xname <- basename(x)

    if (hashtag_rm) {
        xname <- gsub("\\S*#\\S*\\s?", "", xname)
    }

    if (make_portable) {
        xname <- brathering::make_portable_filename(xname, make_unique = T)
    }
    if (cut_codec) {
        # rm codec at end of string
        codec_start <- stringr::str_extract(xname, codec_regexpr)
        ext <- tools::file_ext(xname)
        xname <- tools::file_path_sans_ext(xname)
        name_wo_codec <- sapply(strsplit(xname, codec_start), "[", 1)
        name_wo_codec <- gsub("\\($", "", name_wo_codec) # rm trailing bracket if !make_portable
        xname <- paste0(name_wo_codec, ".", ext)
        xname <- stringr::str_squish(xname)
        xname[which(xname == "")] <- "x"
        xname <- make.unique(xname)
    }
    x_new <- file.path(xdir, xname)
    # rename original files
    fs::file_move(x, x_new)

    # make one argument list for each file
    video_to_video_args_list <- brathering::recycle(short = list(video_to_video_args), long = x_new)
    names(video_to_video_args_list) <- basename(x_new)

    # add paths to list of argument list
    for (i in seq_along(video_to_video_args_list)) {
        video_to_video_args_list[[i]][["x"]] <- x_new[i]
    }

    # get bitrate
    #media_info_df <- get_media_info(x_new)
    media_info_df <- media_info(x_new)

    ## manage fps reduction
    if (!is.null(fps_reduce_to)) {
        vid_to_red_fps <- media_info_df |>
            dplyr::filter(key2 == "video_r_frame_rate") |>
            #dplyr::mutate(value = as.numeric(strsplit(value, "/")[[1]][1])) |>
            #dplyr::rowwise() |>
            dplyr::mutate(value = eval(parse(text = value))) |>
            dplyr::filter(value > fps_reduce_to) |>
            dplyr::mutate(file = basename(file)) |>
            dplyr::pull(file)
        for (i in vid_to_red_fps) {
            video_to_video_args_list[[i]][["fps_out"]] <- fps_reduce_to
            video_to_video_args_list[[i]][["fps_sample"]] <- fps_reduce_to
        }
    }

    if (!is.null(width_reduce_to) || !is.null(height_reduce_to)) {
        widths <- media_info_df |>
            dplyr::filter(key2 == "video_width") |>
            dplyr::mutate(value = as.numeric(value)) |>
            dplyr::pull(value)
        heights <- media_info_df |>
            dplyr::filter(key2 == "video_height") |>
            dplyr::mutate(value = as.numeric(value)) |>
            dplyr::pull(value)
        if (!is.null(width_reduce_to) && !is.null(height_reduce_to)) {
            widths2 <- pmin(widths, width_reduce_to)
            heights2 <- pmin(heights, height_reduce_to)
        } else if (!is.null(width_reduce_to)) {
            widths2 <- pmin(widths, width_reduce_to)
            heights2 <- floor(widths2/widths*heights)
        } else if (!is.null(height_reduce_to)) {
            heights2 <- pmin(heights, height_reduce_to)
            widths2 <- floor(heights2/heights*widths)
        }
        for (i in seq_along(video_to_video_args_list)) {
            video_to_video_args_list[[i]][["scale"]] <- c(widths2[i], heights2[i])
        }
    }

    message("undo renaming with:")
    print(substitute(fs::file_move(x_new, x)))


    for (i in video_to_video_args_list) {
        do.call(video_to_video, args = i)
    }

    return(stats::setNames(x, x_new))
}


