#' Household tasks distribution by gender and arrangement
#'
#' A dataset containing the distribution of household tasks among different arrangements: Wife, Alternating, Husband, and Jointly. The data represents the frequency of each task performed by each arrangement.
#'
#' @docType data
#' @name housetasks
#' @usage data(housetasks)
#' @format A `data.frame` with 13 rows (tasks) and 4 columns (arrangements):
#' \describe{
#'   \item{Wife}{Numeric, the frequency of the task performed primarily by the wife.}
#'   \item{Alternating}{Numeric, the frequency of the task performed in an alternating manner.}
#'   \item{Husband}{Numeric, the frequency of the task performed primarily by the husband.}
#'   \item{Jointly}{Numeric, the frequency of the task performed jointly by both partners.}
#' }
#'
#' @source The dataset was downloaded from the `ggpubr` GitHub repository:
#'   \url{https://raw.githubusercontent.com/kassambara/ggpubr/refs/heads/master/inst/demo-data/housetasks.txt}
#'
#' @examples
#' data(housetasks)
#' head(housetasks)
"housetasks"
