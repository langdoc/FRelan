#' read_tier2 Function read individual tiers from single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' read_tier2(path = "corpora/kpv/", tier = "wordT")

read_tier <- function(eaf_file = "../FRelan/data/input/kpv_izva19570000-291_1a-Brikalansk.eaf", linguistic_type = "wordT", participant = "PXT-F-19XX"){

        `%>%` <- dplyr::`%>%`

        file <- xml2::read_xml(eaf_file)

#        file %>% xml2::xml_find_all(paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")) %>%
#                xml2::xml_attr("PARTICIPANT") -> participant

                dplyr::data_frame(
                        Content = file %>%
                                  xml2::xml_find_all(
                                          paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                                            participant,"']/ANNOTATION/*/ANNOTATION_VALUE")) %>%
                                  xml2::xml_text(),
                        annot_id = file %>%
                                  xml2::xml_find_all(paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                                            participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                                  xml2::xml_attr("ANNOTATION_ID"),
                        ref_id = file %>%
                                  xml2::xml_find_all(
                                          paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                                 participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                                  xml2::xml_attr("ANNOTATION_REF"),
                        ts1 = file %>%
                                xml2::xml_find_all(
                                        paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                               participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                                xml2::xml_attr("TIME_SLOT_REF1"),
                        ts2 = file %>%
                                xml2::xml_find_all(
                                        paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                               participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                                xml2::xml_attr("TIME_SLOT_REF2"),
                        participant = file %>%
                                  xml2::xml_find_all(
                                          paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                                 participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                  xml2::xml_attr("PARTICIPANT"),
                        tier_id = file %>%
                                xml2::xml_find_all(
                                        paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                               participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                xml2::xml_attr("TIER_ID"),
                        type = file %>%
                                xml2::xml_find_all(
                                        paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='",
                                                participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                xml2::xml_attr("LINGUISTIC_TYPE_REF"))
}

# read_tier("../FRelan/data/input/kpv_izva19570000-291_1a-Brikalansk.eaf", linguistic_type = "refT", participant = "PXT-F-19XX")
