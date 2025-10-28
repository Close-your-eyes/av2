#' Title
#'
#' @returns
#' @export
#'
#' @examples
check_ffmpeg <- function() {
    # paths <- system("echo $PATH | tr ':' '\n'", intern = T)
    # allbins <- unique(unlist(purrr::map(paths, list.files)))
    # ffmpegs <- grep("ffmpeg", allbins, value = T)
    grep("ffmpeg", unique(unlist(lapply(system("echo $PATH | tr ':' '\n'", intern = T), list.files))), value = T)
}
