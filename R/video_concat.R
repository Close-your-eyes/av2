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
                         overwrite = F,
                         which_ffmpeg = "ffmpeg",
                         hardcode_chapter = F,
                         add_chapter = T) {

    if (!requireNamespace("brathering", quietly = T)) {
        devtools::install_github("Close-your-eyes/brathering")
    }

    ffmpegs <- check_ffmpeg()
    if (!length(ffmpegs)) {
        message("FFmpeg not installed or not in PATH.")
        message("Install with macports: sudo port install ffmpeg")
        return(NULL)
    }
    ff <- rlang::arg_match(which_ffmpeg, values = ffmpegs)

    # https://trac.ffmpeg.org/wiki/Concatenate
    # procedure = c("demuxer", "protocol")


    #### https://trac.ffmpeg.org/wiki/Concatenate#protocol
    # this is done here
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
                           ".", ext)
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


    ## prepare chapters
    if (add_chapter || hardcode_chapter) {
        chapterdata <- prepare_chapter_metadata_file(videofile_paths = vidfiles)
    }

    metadata_flags <- " "
    if (add_chapter) {
        metadata_flags <- paste0(" -i ", chapterdata[[1]], " -map_metadata 1 ")
        vroom::vroom_write_lines(x = chapterdata[[2]], file = chapterdata[[1]])
    }

    # when no vf: copy, else copy is replaced
    vf_flag <- "-c copy "
    if (hardcode_chapter) {
        vf_flag <- "-vf \""
        for (i in seq_along(chapterdata[[3]])) {
            vf_flag <- paste0(vf_flag, " drawtext=text='", i, "':enable='between(t,", chapterdata[[3]][i]/1000, ",", chapterdata[[4]][i]/1000, ")':x=10:y=10:fontcolor=white:fontsize=36:box=1:boxcolor=black@0.5,")
        }
        vf_flag <- sub(",$", "", vf_flag)
        vf_flag <- paste0(vf_flag, "\" ")
    }

    # make ts first
    purrr::map2(
        vidfiles,
        vidfiles_ts,
        ~system(paste0(ff, " -i ", .x, " -c copy ", .y), intern = T)
    )

    # then concat
    system(
        paste0(ff, " -i 'concat:", paste(vidfiles_ts, collapse = "|"),"'", metadata_flags, vf_flag, overwrite, " ", output),
        intern = T
    )

    # then add chapters
    # ffmpeg -i input.mp4 -i chapters.txt -map_metadata 1 -codec copy output_with_chapters.mp4
    # system(paste0("ffprobe -i ", output, " -show_chapters -loglevel error"))

    if (add_chapter) {
        file.remove(chapterdata[[1]])
    }

    y <- file.remove(vidfiles_ts)
}

prepare_chapter_metadata_file <- function(videofile_paths,
                                          name = "ffmpeg_chapter_temp.txt",
                                          path = tempdir()) {

    txfile <- file.path(path, name)
    times <- av2:::media_info(videofile_paths) |>
        dplyr::filter(stream == "video") |>
        dplyr::filter(key == "duration")

    times2 <- as.numeric(times$value)*1000
    endtimes <- cumsum(times2)
    starttimes <- c(0, endtimes[-length(endtimes)])
    linevec <- ";FFMETADATA1"
    for (i in seq_along(starttimes)) {
        linevec <- c(linevec, "[CHAPTER]", "TIMEBASE=1/1000", paste0("START=",starttimes[i]),
                     paste0("END=",endtimes[i]), paste0("title=", i), "\n")
    }

    return(list(txfile, linevec, starttimes, endtimes))
}
