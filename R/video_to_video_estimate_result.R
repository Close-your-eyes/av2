#' Estimate filesize and conversion time by different settings
#'
#' All combination of preset x quality_crf are tested
#'
#' @param x path to video file
#' @param preset preset flags to iterate
#' @param quality_crf crf flag to iterate
#' @param sample_number how many sample per crf/preset combination
#' @param sample_duration duration in sec of each sample
#' @param video_to_video_args further arguments to av2::video_to_video
#'
#' @returns list
#' @export
#'
#' @examples
video_to_video_estimate_result <- function(x,
                                           preset = c("ultrafast", "superfast", "veryfast", "faster", "fast", "medium",
                                                      "slow", "slower", "veryslow"),
                                           quality_crf = 28,
                                           sample_number = 10,
                                           sample_duration = 10,
                                           video_to_video_args = list()) {

    preset <- rlang::arg_match(preset, multiple = T)
    video_to_video_args <- c(video_to_video_args, list(x = x, cmd_to_key = "comment", runtime_to_key = "composer"))
    info <- av2::media_info(x)

    totalsec <- info |>
        dplyr::filter(key2 == "format_duration") |>
        dplyr::mutate(value = as.numeric(value)) |>
        dplyr::pull(value)

    sample_starts <- sample(1:floor(totalsec)-sample_duration, sample_number)


    out_names <- character(0)
    for (i in preset) {
        for (j in quality_crf) {
            for (k in sample_starts) {
                out_name <- paste0(basename(x), "_", i, "_", j, "_", k)
                cmd <- do.call(video_to_video, args = c(video_to_video_args, list(preset = i,
                                                                                  quality_crf = j,
                                                                                  start = k,
                                                                                  duration = sample_duration)))
                cmds <- strsplit(cmd, "'")[[1]]
                out_names <- c(out_names, cmds[length(cmds)])
            }
        }
    }

    # check results
    df <-
        get_metadata_extended(x = out_names) |>
        dplyr::mutate(runtime_s = round(as.numeric(purrr::map_chr(out_names, get_metadata_tag, tag = "composer")), 3)) |>
        dplyr::mutate(runtime_s = ifelse(runtime_s < 0, 0, runtime_s)) |>
        dplyr::mutate(totalsec = totalsec, duration = as.numeric(duration), size_megabits = as.numeric(size_megabits)) |>
        dplyr::mutate(fraction = duration/totalsec)

    df_summary <- df |>
        dplyr::summarise(size_megabyte_expected = mean(size_megabits*(1/fraction))/8,
                         runtime_expected_min = mean(runtime_s*(1/fraction))/60,
                         size_megabyte_mean = mean(size_megabits)/8,
                         size_megabyte_sd = stats::sd(size_megabits)/8,
                         runtime_s_mean = mean(runtime_s)/60,
                         runtime_s_sd = stats::sd(runtime_s)/60,
                         .by = c(preset, crf))

    fs::file_delete(out_names)
    return(list(df = df, summary = df_summary))
}
