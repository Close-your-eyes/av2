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
    if (!length(tag)) {
        return(NULL)
    }
    tag <- strsplit(tag, "\"")[[1]][2]
    return(tag)
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

#' Extract Media File Metadata with FFprobe
#'
#' Uses FFprobe to extract format and stream metadata from one or more media
#' files. Results can be returned in long or wide format. Derived fields for
#' file size in megabits and stream bitrate in megabits per second are added
#' when the corresponding FFprobe values are available.
#'
#' @param x A character vector containing media file paths or a directory whose
#'   media files should be inspected. The value is passed to
#'   [x_dir_or_files()].
#' @param return Output format. `"long"` returns one row per metadata field;
#'   `"wide"` returns one row per file with metadata fields represented as
#'   columns.
#' @param rm_constant_cols Logical. When `TRUE` and `return = "wide"`, remove
#'   columns containing the same value for every file. Defaults to `FALSE`.
#'
#' @return A tibble. In long format, the result contains the columns `file`,
#'   `stream`, `key`, `value`, and `key2`. In wide format, the result contains
#'   one row per file and one column per metadata field. Columns containing only
#'   missing values are removed from wide output.
#'
#' @details
#' This function requires the `ffprobe` command-line program to be installed
#' and available on the system path. Values reported by FFprobe as `"N/A"` are
#' converted to `NA`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Return metadata in long format
#' media_info("video.mp4")
#'
#' # Return one row per media file
#' media_info(
#'   c("video.mp4", "audio.mp3"),
#'   return = "wide"
#' )
#'
#' # Inspect a directory and remove invariant columns
#' media_info(
#'   "path/to/media",
#'   return = "wide",
#'   rm_constant_cols = TRUE
#' )
#' }
media_info <- function(x,
                       return = c("long", "wide"),
                       rm_constant_cols = F) {

    return <- rlang::arg_match(return)
    x <- x_dir_or_files(x)

    df <- purrr::map_dfr(purrr::set_names(x), function(y) {
        info <- system(paste0("ffprobe -v error -print_format flat -show_format -show_streams ", shQuote(y)), intern = T)

        # Turn into data frame
        df <- tibble::tibble(raw = info) |>
            tidyr::separate(raw, into = c("key", "value"), sep = "=", extra = "merge") |>
            dplyr::mutate(key = gsub("streams\\.stream\\.", "stream", key)) |>
            tidyr::separate(key, into = c("stream", "key"), sep = "\\.", extra = "merge") |>
            dplyr::mutate(value = gsub("\"", "", value))
        size <- as.numeric(df[intersect(which(df$stream == "format"), which(df$key == "size")),"value",drop = T])
        df <- rbind(df, data.frame(stream = "format", key = "size_megabits", value = as.character(round(size*8/1e6, 2))))

        for (i in unique(df$stream)) {
            bitrate <- as.numeric(df[intersect(which(df$stream == i), which(df$key == "bit_rate")),"value",drop = T])
            df <- rbind(df, data.frame(stream = i, key = "bit_rate_mbps", value = as.character(round(bitrate/1e6, 2))))
        }

        streamdef <- df |> dplyr::filter(key == "codec_type")
        streamdef <- stats::setNames(streamdef$value, streamdef$stream)
        missing <- setdiff(unique(df$stream), names(streamdef))
        names(missing) <- missing
        streamdef <- c(streamdef, missing)
        df$stream <- streamdef[df$stream]

        # use aspect_ratio
        # why h/w was wrong?
        # h <- as.numeric(df[intersect(which(df$stream == "video"), which(df$key == "height")),"value",drop = T])
        # w <- as.numeric(df[intersect(which(df$stream == "video"), which(df$key == "width")),"value",drop = T])
        # df <- rbind(df, data.frame(stream = "video", key = "orientation", value = ifelse(h/w>1, "portrait", ifelse(h/w<1, "landscape", "square"))))

        df[["key2"]] <- paste0(df$stream, "_", df$key)

        return(df)

    }, .id = "file")

    df <- dplyr::mutate(df, value = ifelse(value == "N/A", NA, value))

    # nb_frames are total frames
    if (return == "wide") {
        df <- brathering::df_cols_to_numeric(df |>
                                                 dplyr::select(file, value, key2) |>
                                                 tidyr::pivot_wider(names_from = key2, values_from = value))

        df <- brathering::df_rm_na_cols(df)
        if (rm_constant_cols) {
            df <- brathering::df_rm_eq_cols(df)
        }
    }

    return(df)
}


#' #' Get media info in long data frame format
#' #'
#' #' None video files in x may cause errors.
#' #'
#' #' @param x directory with video files or vector of file paths
#' #'
#' #' @returns
#' #' @export
#' #'
#' #' @examples
#' get_media_info <- function(x) {
#'
#'     x <- x_dir_or_files(x)
#'     df <- fs::file_info(x)
#'     # file.info(x)$size
#'     media_info_list <- purrr::map(stats::setNames(x, basename(x)), av::av_media_info)
#'
#'     n_frames <- floor(purrr::map_dbl(media_info_list, ~purrr::pluck(.x, "video", "framerate"))*
#'                           purrr::map_dbl(media_info_list, ~purrr::pluck(.x, "duration")))
#'
#'     size_megabits <- as.numeric(df$size)*8/1e6
#'     bitrate_mbps <- size_megabits/purrr::map_dbl(media_info_list, `[[`, "duration")
#'     bitrate_mbps <- round(bitrate_mbps, 2)
#'
#'     for (i in seq_along(media_info_list)) {
#'         media_info_list[[i]][["duration"]] <- data.frame(media_info_list[[i]]["duration"])
#'         media_info_list[[i]][["video"]][["bitrate_mbps"]] <- as.character(bitrate_mbps[i])
#'         media_info_list[[i]][["duration"]][["size_megabits"]] <- as.character(size_megabits[i])
#'         if (is.na(media_info_list[[i]][["video"]][["frames"]])) {
#'             media_info_list[[i]][["video"]][["frames"]] <- n_frames[i]
#'         }
#'     }
#'     media_info_df <- purrr::map_dfr(media_info_list, media_info_to_long, .id = "file")
#'     return(media_info_df)
#' }
#'
#' media_info_to_long <- function(x) {
#'
#'     video_df <- x$video |>
#'         dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
#'         tidyr::pivot_longer(cols = dplyr::everything(), names_to = "attribute", values_to = "value") |>
#'         dplyr::mutate(source = "video")
#'     if (!is.null(x$audio)) {
#'         audio_df <- x$audio |>
#'             dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
#'             tidyr::pivot_longer(cols = dplyr::everything(), names_to = "attribute", values_to = "value") |>
#'             dplyr::mutate(source = "audio")
#'     } else {
#'         audio_df <- NULL
#'     }
#'     duration_long <- x$duration |> #data.frame(duration = x$duration) |>
#'         dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
#'         tidyr::pivot_longer(cols = dplyr::everything(), names_to = "attribute", values_to = "value") |>
#'         dplyr::mutate(source = "duration")
#'
#'     # Combine all into one long data frame
#'     long_df <- dplyr::bind_rows(duration_long, video_df, audio_df)
#'     return(long_df)
#' }




