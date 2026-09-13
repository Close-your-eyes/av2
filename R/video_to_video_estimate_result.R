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
#' \dontrun{
#' tt <- video_to_video_estimate_result("/Users/chris/Downloads/The_Worlds_Most_Important_Machine.mkv",
#'                                      sample_duration = 20,
#'                                      quality_crf = seq(24,36,4),
#'                                      preset = c("ultrafast", "superfast", "veryfast", "faster", "fast", "medium",
#'                                                 "slow", "slower", "veryslow"),
#'                                      video_to_video_args = list(encoder = "libx265"))
#'
#'     |preset    |crf | size_megabyte_expected| runtime_expected_min| size_megabyte_mean| size_megabyte_sd| runtime_s_mean| runtime_s_sd|
#'     |:---------|:---|----------------------:|--------------------:|------------------:|----------------:|--------------:|------------:|
#'     |ultrafast |24  |                 238.58|               0.0000|             1.4476|           0.5087|         0.0000|       0.0000|
#'     |ultrafast |28  |                 162.15|               0.0000|             0.9839|           0.3132|         0.0000|       0.0000|
#'     |ultrafast |32  |                 115.47|               0.0000|             0.7006|           0.1950|         0.0000|       0.0000|
#'     |ultrafast |36  |                  88.11|               0.0000|             0.5346|           0.1197|         0.0000|       0.0000|
#'     |superfast |24  |                 248.97|               0.5494|             1.5106|           0.5489|         0.0033|       0.0070|
#'     |superfast |28  |                 171.32|               0.2747|             1.0395|           0.3505|         0.0017|       0.0053|
#'     |superfast |32  |                 123.90|               0.2747|             0.7518|           0.2251|         0.0017|       0.0053|
#'     |superfast |36  |                  95.73|               0.0000|             0.5809|           0.1414|         0.0000|       0.0000|
#'     |veryfast  |24  |                 262.38|               3.5709|             1.5920|           0.5788|         0.0217|       0.0081|
#'     |veryfast  |28  |                 179.23|               3.2962|             1.0875|           0.3721|         0.0200|       0.0070|
#'     |veryfast  |32  |                 127.93|               3.0215|             0.7762|           0.2380|         0.0183|       0.0053|
#'     |veryfast  |36  |                  97.05|               2.7468|             0.5889|           0.1457|         0.0167|       0.0000|
#'     |faster    |24  |                 262.25|               3.5709|             1.5913|           0.5779|         0.0217|       0.0081|
#'     |faster    |28  |                 179.11|               3.0215|             1.0868|           0.3720|         0.0183|       0.0053|
#'     |faster    |32  |                 127.91|               3.0215|             0.7761|           0.2372|         0.0183|       0.0053|
#'     |faster    |36  |                  96.95|               2.7468|             0.5882|           0.1455|         0.0167|       0.0000|
#'     |fast      |24  |                 270.82|               4.9443|             1.6432|           0.5826|         0.0300|       0.0070|
#'     |fast      |28  |                 186.87|               4.3949|             1.1339|           0.3750|         0.0267|       0.0086|
#'     |fast      |32  |                 134.88|               3.8456|             0.8184|           0.2409|         0.0233|       0.0086|
#'     |fast      |36  |                 102.86|               3.5709|             0.6241|           0.1488|         0.0217|       0.0081|
#'     |medium    |24  |                 276.84|               5.2190|             1.6798|           0.6009|         0.0317|       0.0095|
#'     |medium    |28  |                 187.60|               4.1202|             1.1382|           0.3815|         0.0250|       0.0088|
#'     |medium    |32  |                 132.82|               3.5709|             0.8059|           0.2427|         0.0217|       0.0081|
#'     |medium    |36  |                  99.94|               3.0215|             0.6064|           0.1492|         0.0183|       0.0053|
#'     |slow      |24  |                 302.72|              13.4595|             1.8367|           0.6476|         0.0817|       0.0200|
#'     |slow      |28  |                 203.81|              11.2620|             1.2366|           0.4056|         0.0683|       0.0166|
#'     |slow      |32  |                 142.56|              10.1633|             0.8650|           0.2562|         0.0617|       0.0158|
#'     |slow      |36  |                 105.52|               8.7899|             0.6402|           0.1578|         0.0533|       0.0153|
#'     |slower    |24  |                 301.05|              65.6494|             1.8266|           0.6521|         0.3983|       0.0941|
#'     |slower    |28  |                 204.84|              54.6621|             1.2429|           0.4115|         0.3317|       0.0760|
#'     |slower    |32  |                 143.47|              45.5975|             0.8705|           0.2595|         0.2767|       0.0658|
#'     |slower    |36  |                 105.21|              39.0051|             0.6384|           0.1581|         0.2367|       0.0526|
#'     |veryslow  |24  |                 300.76|             115.0925|             1.8249|           0.6493|         0.6983|       0.1775|
#'     |veryslow  |28  |                 204.59|              96.9634|             1.2414|           0.4091|         0.5883|       0.1534|
#'     |veryslow  |32  |                 143.06|              81.5811|             0.8680|           0.2575|         0.4950|       0.1277|
#'     |veryslow  |36  |                 104.70|              68.3963|             0.6352|           0.1561|         0.4150|       0.1026|
#' }
video_to_video_estimate_result <- function(x,
                                           preset = c("ultrafast", "superfast", "veryfast", "faster", "fast", "medium",
                                                      "slow", "slower", "veryslow"),
                                           quality_crf = 28,
                                           sample_number = 10,
                                           sample_duration = 10,
                                           video_to_video_args = list(log_level = "quiet")) {

    preset <- rlang::arg_match(preset, multiple = T)
    video_to_video_args <- c(video_to_video_args, list(x = x, cmd_to_key = "comment", runtime_to_key = "composer"))
    info <- av2::media_info(x)

    totalsec <- info |>
        dplyr::filter(key2 == "format_duration") |>
        dplyr::mutate(value = as.numeric(value)) |>
        dplyr::pull(value)

    sample_starts <- sample(1:floor(totalsec)-sample_duration, sample_number)


    out_names <- character(0)
    settings <- character(0)
    iters <- length(preset)*length(quality_crf)*length(sample_starts)
    message(iters, " conversions are made.")
    counter <- 0
    for (i in preset) {
        for (j in quality_crf) {
            for (k in sample_starts) {
                counter <- counter+1
                message(counter, " / ", iters)
                # out_name <- paste0(basename(x), "_", i, "_", j, "_", k)
                settings <- c(settings, paste0(i, "_", j))
                cmd <- do.call(video_to_video, args = c(video_to_video_args, list(preset = i,
                                                                                  quality_crf = j,
                                                                                  start = k,
                                                                                  duration = sample_duration)))
                cmds <- strsplit(cmd, "'")[[1]]
                out_names <- c(out_names, cmds[length(cmds)])
            }
        }
    }


    settings <- strsplit(settings, "_")
    # check results
    tt0 <- media_info(x, return = "wide")
    tt <- media_info(out_names, return = "wide")
    df <- tt |>
        dplyr::mutate(runtime_s = round(as.numeric(format_tags.COMPOSER))) |>
        dplyr::mutate(runtime_s = ifelse(runtime_s < 0, 0, runtime_s)) |>
        dplyr::mutate(fraction = format_duration/tt0$format_duration) |>
        dplyr::mutate(preset = sapply(settings, "[", 1), crf = sapply(settings, "[", 2))

    df_summary <- df |>
        dplyr::summarise(size_megabyte_expected = mean(format_size_megabits*(1/fraction))/8,
                         runtime_expected_min = mean(runtime_s*(1/fraction))/60,
                         size_megabyte_mean = mean(format_size_megabits)/8,
                         size_megabyte_sd = stats::sd(format_size_megabits)/8,
                         runtime_s_mean = mean(runtime_s)/60,
                         runtime_s_sd = stats::sd(runtime_s)/60,
                         .by = c(preset, crf))

    fs::file_delete(out_names)
    return(list(df = df, summary = df_summary))
}
