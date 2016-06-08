#' read_eaf Function
#'
#' This is an updated version of the old function, which has been renamed to read_eaf_old(). Instead of trying to do everything this function is supposedly used with `plyr` in order to parse multiple files. The function parses ELAN files. Dummy tokens that contain no information are erased automatically. The files that are for some reason not parsable are skipped. In actual use the biggest problems are connected to structural irregularity of ELAN files in corpus. It is used ideally in connection with scripts that are able to parse IMDI or CMDI files. Please use `log_eaf()` function to see which files have been changed recently, those are usually the ones containing problems.
#' @param eaf_file The path to ELAN file which we want to parse
#' @param def_tier Linguistic type of the independent tier
#' @param sa_tier Linguistic type of the Symbolic Association tier
#' @param ss_tier Linguistic type of the Symbolic Subdivision tier (usually contains the tokenized wordforms)
#' @keywords ELAN
#' @export
#' @examples
#' read_eaf(eaf_file = "corpora/kpv/session_1.eaf", ind_tier = "refT", sa_tier = "orthT", ss_tier = "wordT")

read_eaf <- function(eaf_file = "data/kpv_izva/kpv_izva18440000Castren-2.eaf", ind_tier = "refT", sa_tier = "orthT", ss_tier = "wordT"){

        `%>%` <- dplyr::`%>%`

        eaf = xml2::read_xml(eaf_file)

        eaf <- FRelan::read_tier(xml_object = eaf, read_file = F, linguistic_type = ss_tier) %>%
                dplyr::select(content, ref_id, participant) %>%
                dplyr::rename(token = content) %>%
                dplyr::rename(annot_id = ref_id) %>%
                dplyr::left_join(FRelan::read_tier(xml_object = eaf, read_file = F, linguistic_type = sa_tier)) %>%
                dplyr::select(token, content, participant, ref_id) %>%
                dplyr::rename(utterance = content) %>%
                dplyr::rename(annot_id = ref_id) %>%
                dplyr::left_join(FRelan::read_tier(xml_object = eaf, read_file = F, linguistic_type = ind_tier)) %>%
                dplyr::select(token, utterance, content, participant, time_slot_1, time_slot_2) %>%
                dplyr::rename(reference = content) %>% dplyr::left_join(FRelan::read_timeslots(xml_object = eaf, read_file = F),
                                                                        by = c("time_slot_1" = "time_slot_id")) %>%
                dplyr::rename(time_start = time_value) %>%
                dplyr::left_join(FRelan::read_timeslots(xml_object = eaf, read_file = F),
                                 by = c("time_slot_2" = "time_slot_id")) %>%
                dplyr::rename(time_end = time_value) %>%
                dplyr::select(-time_slot_1, -time_slot_2) -> eaf

        eaf$session_name <- gsub(".+/(.+).eaf", "\\1", eaf_file)
        eaf$filename <- eaf_file

#        eaf %>% FRelan::add_kwic() -> eaf

        eaf
}
