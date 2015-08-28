#' find_token Function
#'
#' This function is equivalent to `filter(grepl("regex", Token))`, but demands less typing. It is convenient to use this function in a dplyr pipe after the data frame output of read_eaf() function. The results can be, as an example, piped into FRorpus() function in order to view the results in a Shiny application.
#' @param corpus This is a data frame which contains column `Token`.
#' @param regex This is a regex one searches with
#' @keywords ELAN
#' @export
#' @examples
#'find_token("^nd.+")

find_token <- function(corpus = corpus, regex = "test") {
        library(dplyr)
        corpus %>% dplyr::filter(grepl(regex, Token))
}
