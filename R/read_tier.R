#' read_tier Function read individual tiers from single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' read_tier(path = "corpora/kpv/", tier = "wordT")

read_tier <- function(eaf_file = "/Volumes/langdoc/langs/kpv/kpv_izva20140404IgusevJA/kpv_izva20140404IgusevJA.eaf", participant = "JAI-M-1939", linguistic_type = "wordT", independent = F){

                `%>%` <- dplyr::`%>%`

                file <- xml2::read_xml(eaf_file)

                # file %>% xml2::xml_find_all(paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")) %>%
                #                 xml2::xml_attr("PARTICIPANT") -> participants_in_file
                #
                # participant %in% participants_in_file

                create_path <- function(..., above = F){
                        if (exists("participant")){
                                restriction <- paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "' and @PARTICIPANT='", participant,"']")
                        } else {
                                restriction <- paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")
                        }
                        if (above == T){
                                xpath_end <- "/ANNOTATION/*/ANNOTATION_VALUE/../../.."
                        } else {
                                xpath_end <- "/ANNOTATION/*/ANNOTATION_VALUE"
                        }

                        paste0(restriction, xpath_end)
                }


                dplyr::data_frame(
                        Content = file %>% xml2::xml_find_all(create_path()) %>% xml2::xml_text(),
                        annot_id = file %>% xml2::xml_find_all(paste0(create_path(), "/..")) %>% xml2::xml_attr("ANNOTATION_ID"),
                        ref_id = file %>% xml2::xml_find_all(paste0(create_path(), "/..")) %>% xml2::xml_attr("ANNOTATION_REF"),
                        participant = file %>% xml2::xml_find_all(create_path(above = T)) %>% xml2::xml_attr("PARTICIPANT"),
                        tier_id = file %>% xml2::xml_find_all(create_path(above = T)) %>% xml2::xml_attr("TIER_ID"),
                        type = file %>% xml2::xml_find_all(create_path(above = T)) %>% xml2::xml_attr("LINGUISTIC_TYPE_REF"))
        }
