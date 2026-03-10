#' Convert Strings to Title Case
#'
#' Converts the first character of each string to uppercase and the rest to lowercase.
#'
#' @param x A character vector or a list containing strings to convert to title case.
#'
#' @return A character vector with the same length as `x`, where each element
#' has its first character converted to uppercase and remaining characters are preserved as-is.
#'
#' @examples
#' to_title(c("hELLO", "WoRLD", "R"))
#' # Returns: "Hello" "World" "R"
#'
#' @export
to_title <- function(x) {
    lapply(
        x,
        function(i) {
            if (!is.na(i) && !is.null(i)) {
                paste0(toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)))
            } else {
                i
            }
        }
    ) %>% unlist()
}
