#' read_tier Function read individual tiers from single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' read_tier(path = "corpora/kpv/", tier = "wordT")

read_tier <- function(eaf_file = "/Volumes/langdoc/langs/kpv/kpv_izva20140404IgusevJA/kpv_izva20140404IgusevJA.eaf", linguistic_type = "wordT"){

                `%>%` <- dplyr::`%>%`

                file <- xml2::read_xml(eaf_file)

                file %>% xml2::xml_find_all(paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")) %>%
                        xml2::xml_attr("PARTICIPANT") -> participants_in_file

                coerce_data_frame <- function(participant){
                        dplyr::data_frame(
                                content = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE")) %>%
                                        xml2::xml_text(),
                                annot_id = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                                        xml2::xml_attr("ANNOTATION_ID"),
                                ref_id = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE/..")) %>%
                                        xml2::xml_attr("ANNOTATION_REF"),
                                participant = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                        xml2::xml_attr("PARTICIPANT"),
                                tier_id = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                        xml2::xml_attr("TIER_ID"),
                                type = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                        xml2::xml_attr("LINGUISTIC_TYPE_REF"),
                                time_slot_1 = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                        xml2::xml_attr("TIME_SLOT_REF_1"),
                                time_slot_2 = file %>%
                                        xml2::xml_find_all(
                                                paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']/ANNOTATION/*/ANNOTATION_VALUE/../../..")) %>%
                                        xml2::xml_attr("TIME_SLOT_REF_2"))
                }

                plyr::ldply(participants_in_file, coerce_data_frame) %>% dplyr::tbl_df()

        }
