#' check_words Function
#'
#' This function takes one ELAN file reads the word tokens and compares those to the previously created data frame.
#' The idea is to find tokens which do not occur in corpus already and are possible candidates for being typos.
#' @param eaf_file Filename
#' @param corpus Data frame which contains corpus to compare
#' @keywords ELAN
#' @export
#' @examples
#' check_words(eaf_file = "file.eaf", corpus_rds = "corpus.rds")

check_words <- function(eaf_file, corpus_rds){

        corpus_to_compare <- readr::read_rds(corpus_rds) %>% tbl_df
        new_words <- FRelan::read_tier(eaf_file = eaf_file, linguistic_type = "wordT") %>% .$content
        new_words <- tolower(new_words)
        sort(new_words[! new_words %in% corpus_to_compare$token])

}
