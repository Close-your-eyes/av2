#' Concatenate Video Files
#'
#' Join video files from a directory in file-name order using FFmpeg, with
#' optional chapter metadata and visible chapter numbers.
#'
#' @param x Character string giving an existing directory containing the input
#'   videos. Files should have the same extension and compatible streams. Name
#'   files in the intended playback order, for example, `clip01.mp4`,
#'   `clip02.mp4`, and `clip03.mp4`.
#' @param out_path Output directory. Must already exist. If `NULL`, use `x`.
#' @param out_name Output file name. If `NULL`, combine the first and last input
#'   file names without their extensions, separated by `___`, and append the
#'   input extension. If supplied without an extension, the input extension is
#'   appended. A supplied extension must match the input extension.
#' @param overwrite Logical; overwrite an existing output file? If `FALSE`,
#'   create a unique output path using `brathering::make_filepath_unique()`.
#' @param which_ffmpeg FFmpeg executable name, selected from the names returned
#'   by `check_ffmpeg()`. Defaults to `"ffmpeg8"`, which must be available on
#'   `PATH` when this default is used.
#' @param hardcode_chapter Logical; burn sequential chapter numbers into the
#'   upper-left corner of the video? Each input file defines one chapter.
#'   If `TRUE`, video is re-encoded instead of using stream copy. Defaults to
#'   `FALSE`.
#' @param add_chapter Logical; add chapter metadata to the output? Each input
#'   file defines one chapter, titled with its sequential number. Defaults to
#'   `TRUE`. Can be used together with `hardcode_chapter`.
#'
#' @details
#' Inputs are listed non-recursively in the order returned by [list.files()].
#' The most frequent file extension is selected, and paths ending in that
#' extension are retained. Keep only the intended inputs in `x`; an existing
#' output in the same directory can be selected on a later call.
#'
#' Each input is first copied into an intermediate MPEG transport stream with
#' the same base name and a `.ts` extension beside the input. These intermediate
#' files are removed after concatenation. Use a directory without existing
#' `.ts` files that could conflict with these paths. Streams are copied to the
#' output unless `hardcode_chapter` is `TRUE`.
#'
#' FFmpeg must be installed and available on `PATH`. Chapter options also
#' require `ffprobe` on `PATH` to read input durations. Adding chapter metadata
#' requires the \pkg{vroom} package; creating a unique output path requires
#' \pkg{brathering}. Paths should not contain spaces or shell metacharacters,
#' because not all paths in the generated commands are shell-quoted.
#'
#' @returns Invisibly, a logical vector indicating whether each intermediate
#'   `.ts` file was removed. This does not indicate whether FFmpeg succeeded.
#'   Returns `NULL` if no FFmpeg executable is found.
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' # Join sequentially named clips and add chapter metadata.
#' video_concat("clips", out_path = "output", out_name = "joined.mp4")
#'
#' # Burn chapter numbers into the video without adding chapter metadata.
#' video_concat(
#'   "clips", out_path = "output", out_name = "numbered.mp4",
#'   hardcode_chapter = TRUE, add_chapter = FALSE
#' )
#' }
video_concat <- function(x,
                         out_path = NULL,
                         out_name = NULL,
                         overwrite = F,
                         which_ffmpeg = "ffmpeg8",
                         hardcode_chapter = F,
                         add_chapter = T) {

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
        av2:::.ensure_package("brathering")
        output <- brathering::make_filepath_unique(output)
    }
    overwrite <- ifelse(overwrite, "-y", "-n")


    ## prepare chapters
    if (add_chapter || hardcode_chapter) {
        chapterdata <- prepare_chapter_metadata_file(videofile_paths = vidfiles)
    }

    metadata_flags <- " "
    if (add_chapter) {
        av2:::.ensure_package("vroom")
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
