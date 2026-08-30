#' Resolve a Directory or File Paths
#'
#' Removes shell quoting from paths and resolves them to existing files. If the
#' first path is a directory, the function returns all entries directly inside
#' that directory. Otherwise, it returns only the supplied paths that correspond
#' to existing files.
#'
#' @param x A character vector containing a directory path or one or more file
#'   paths. Paths may be shell-quoted.
#'
#' @return A character vector of file paths.
#'
#' @details
#' When `x[1]` is a directory, any additional elements of `x` are ignored.
#' Directory contents are returned as full paths and are not listed recursively.
#' An error is raised if no files are found.
#'
#' @examples
#' \dontrun{
#' x_dir_or_files("data")
#' x_dir_or_files(c("data/file1.csv", "data/file2.csv"))
#' x_dir_or_files(shQuote("data/file 1.csv"))
#' }
#'
#' @keywords internal
x_dir_or_files <- function(x) {
    x <- unshquote(x)
    if (fs::is_dir(x[1])) {
        x <- list.files(x, full.names = TRUE)
    } else {
        x <- x[which(fs::file_exists(x))]
        x <- x[!fs::is_dir(x)]
    }
    if (!length(x)) {
        stop("no files found.")
    }
    return(x)
}


#' Detect Shell-Quoted Strings
#'
#' Determines whether strings appear to have been produced by `shQuote()`
#' using either POSIX-style single quoting or Windows-style double quoting.
#'
#' @param x A character vector of strings to inspect.
#'
#' @return A logical vector with one element for each element of `x`. An element
#'   is `TRUE` when the corresponding string looks shell-quoted and `FALSE`
#'   otherwise. Missing inputs produce `NA`.
#'
#' @examples
#' is_shquoted("'file name.txt'")
#' is_shquoted('"file name.txt"')
#' is_shquoted("file name.txt")
#'
#' @seealso [shQuote()], [unshquote()]
#'
#' @keywords internal
is_shquoted <- function(x) {
    # POSIX: '...' (with internal quotes as '\'' sequences)
    posix <- grepl("^'(?:[^']|(?:'\\\\''))*'$", x, perl = TRUE)

    # Windows: "..." with backslash escapes
    win <- grepl('^"(?:[^"\\\\]|\\\\.)*"$', x, perl = TRUE)

    posix | win
}


#' Remove Shell Quoting
#'
#' Removes recognized POSIX- or Windows-style shell quoting from strings and
#' restores escaped quote or backslash characters.
#'
#' @param x A character vector of potentially shell-quoted strings.
#'
#' @return A character vector of the same length as `x`, with recognized outer
#'   quoting and associated escape sequences removed. Unquoted strings are
#'   returned unchanged.
#'
#' @details
#' POSIX strings enclosed in single quotes are unwrapped, and embedded
#' single-quote escape sequences are restored. Windows strings enclosed in
#' double quotes are unwrapped, and escaped double quotes and backslashes are
#' restored.
#'
#' As an additional fallback, strings still enclosed in single quotes after
#' processing have those outer quotes removed.
#'
#' @examples
#' unshquote("'file name.txt'")
#' unshquote('"file name.txt"')
#' unshquote(c("'one.txt'", "two.txt"))
#'
#' @seealso [shQuote()], [is_shquoted()]
#'
#' @keywords internal
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

    # Extra case: "'string'" -> string
    extra <- grepl("^'.*'$", out) & nchar(out) >= 2
    if (any(extra)) {
        out[extra] <- substr(out[extra], 2L, nchar(out[extra]) - 1L)
    }

    return(out)
}
