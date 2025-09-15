#' Get media info in long data frame format
#'
#' None video files in x may cause errors.
#'
#' @param x directory with video files or vector of file paths
#'
#' @returns
#' @export
#'
#' @examples
get_media_info <- function(x) {

    x <- x_dir_or_files(x)
    df <- fs::file_info(x)
    # file.info(x)$size
    media_info_list <- purrr::map(stats::setNames(x, basename(x)), av::av_media_info)

    n_frames <- floor(purrr::map_dbl(media_info_list, ~purrr::pluck(.x, "video", "framerate"))*
                          purrr::map_dbl(media_info_list, ~purrr::pluck(.x, "duration")))

    size_megabits <- as.numeric(df$size)*8/1e6
    bitrate_mbps <- size_megabits/purrr::map_dbl(media_info_list, `[[`, "duration")
    bitrate_mbps <- round(bitrate_mbps, 2)

    for (i in seq_along(media_info_list)) {
        media_info_list[[i]][["duration"]] <- data.frame(media_info_list[[i]]["duration"])
        media_info_list[[i]][["video"]][["bitrate_mbps"]] <- as.character(bitrate_mbps[i])
        media_info_list[[i]][["duration"]][["size_megabits"]] <- as.character(size_megabits[i])
        if (is.na(media_info_list[[i]][["video"]][["frames"]])) {
            media_info_list[[i]][["video"]][["frames"]] <- n_frames[i]
        }
    }
    media_info_df <- purrr::map_dfr(media_info_list, media_info_to_long, .id = "file")
    return(media_info_df)
}

media_info_to_long <- function(x) {

    video_df <- x$video |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "attribute", values_to = "value") |>
        dplyr::mutate(source = "video")
    if (!is.null(x$audio)) {
        audio_df <- x$audio |>
            dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
            tidyr::pivot_longer(cols = dplyr::everything(), names_to = "attribute", values_to = "value") |>
            dplyr::mutate(source = "audio")
    } else {
        audio_df <- NULL
    }
    duration_long <- x$duration |> #data.frame(duration = x$duration) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "attribute", values_to = "value") |>
        dplyr::mutate(source = "duration")

    # Combine all into one long data frame
    long_df <- dplyr::bind_rows(duration_long, video_df, audio_df)
    return(long_df)
}

x_dir_or_files <- function(x) {
    x <- unshquote(x)
    if (fs::is_dir(x[1])) {
        x <- list.files(x, full.names = T)
    } else {
        x <- x[which(fs::file_exists(x))]
    }
    if (!length(x)) {
        stop("no files found.")
    }
    return(x)
}

# Detect whether a string looks like shQuote() output
is_shquoted <- function(x) {
    # POSIX: '...'(with internal quotes as '"'"')
    posix <- grepl("^'(?:[^']|(?:'\\\\''))*'$", x, perl = TRUE)
    # Windows: "..." with backslash escapes
    win   <- grepl('^"(?:[^"\\\\]|\\\\.)*"$', x, perl = TRUE)
    posix | win
}

# Undo shQuote() (vectorized). Leaves non-quoted strings untouched.
unshquote <- function(x) {
    out <- x

    # POSIX shQuote style: 'abc' or 'a'\''b'
    posix <- grepl("^'(?:[^']|(?:'\\\\''))*'$", out, perl = TRUE)
    if (any(posix)) {
        y <- substr(out[posix], 2L, nchar(out[posix]) - 1L)
        y <- gsub("'\\\\''", "'", y, perl = TRUE)
        out[posix] <- y
    }

    # Windows style: "abc" with backslash escapes
    win <- grepl('^"(?:[^"\\\\]|\\\\.)*"$', out, perl = TRUE)
    if (any(win)) {
        y <- substr(out[win], 2L, nchar(out[win]) - 1L)
        y <- gsub('\\\\(["\\\\])', '\\1', y, perl = TRUE)
        out[win] <- y
    }

    # Extra case: "'string'" → string
    extra <- grepl("^'.*'$", out) & nchar(out) >= 2
    if (any(extra)) {
        out[extra] <- substr(out[extra], 2L, nchar(out[extra]) - 1L)
    }

    return(out)
}





media_info <- function(x) {

    df <- purrr::map_dfr(stats::setNames(x, x), function(x) {
        x <- shQuote(x)
        info <- system(paste0("ffprobe -v error -print_format flat -show_format -show_streams ", x), intern = T)

        # Turn into data frame
        df <- tibble::tibble(raw = info) |>
            tidyr::separate(raw, into = c("key", "value"), sep = "=", extra = "merge") |>
            dplyr::mutate(key = gsub("streams\\.stream\\.", "stream", key)) |>
            tidyr::separate(key, into = c("stream", "key"), sep = "\\.", extra = "merge") |>
            dplyr::mutate(value = gsub("\"", "", value))
        streamdef <- df |> dplyr::filter(key == "codec_type")
        streamdef <- stats::setNames(streamdef$value, streamdef$stream)
        missing <- setdiff(unique(df$stream), names(streamdef))
        names(missing) <- missing
        streamdef <- c(streamdef, missing)
        df$stream <- streamdef[df$stream]
        df[["key2"]] <- paste0(df$stream, "_", df$key)
        return(df)
    }, .id = "file")
    return(df)
}
