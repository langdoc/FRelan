#' exc_token Function
#'
#' This function is equivalent to `filter(! grepl("regex", Token))`, but demands less typing. It is convenient to use this function in a dplyr pipe after the data frame output of read_eaf() function. It is very common that in order to exclude some tokens we can just define some regular expression patterns which do the job, after which we pass the results onward.
#' @param corpus This is a data frame which contains column `Token`.
#' @param regex This is a regex one searches wants to exclude
#' @keywords ELAN
#' @export
#' @examples
#'corpus %>% find_token(".+ter$") %>% exc_token("^a.+")

exc_token <- function(corpus = corpus, regex = "test") {
        corpus %>% dplyr::filter(! grepl(regex, token))
}
