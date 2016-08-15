#' check_words Function
#'
#' This function takes one ELAN file reads the word tokens and compares those to the previously created data frame.
#' The idea is to find tokens which do not occur in corpus already and are possible candidates for being typos.
#' @param eaf_file Filename
#' @param corpus Data frame which contains corpus to compare
#' @keywords ELAN
#' @export

check_words <- function(eaf_file = "/Volumes/langdoc/langs/kpv/kpv_izva20150406-3/kpv_izva20150406-3-a/kpv_izva20150406-3-a.eaf", corpus_rds = "/Volumes/langdoc/langs/kpv/kpv_izva_corpus.rds"){

        corpus_to_compare <- readr::read_rds(corpus) %>% tbl_df
        new_words <- FRelan::read_tier(eaf_file = eaf_file, linguistic_type = "wordT") %>% .$content
        new_words <- tolower(new_words)
        sort(new_words[! new_words %in% corpus_to_compare$token])

}
