#' read_lang Function reads tier language attributes from a single ELAN files.
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' read_tier(path = "corpora/kpv/", tier = "wordT")

read_lang <- function(eaf_file = "/Volumes/langdoc/langs/kpv/kpv_izva20140404IgusevJA/kpv_izva20140404IgusevJA.eaf", linguistic_type = "orthT", def_lang = "kpv"){

        `%>%` <- dplyr::`%>%`

        eaf_xml <- xml2::read_xml(eaf_file)
        expr <- paste0("//TIER[@LINGUISTIC_TYPE_REF='", linguistic_type, "']")

        dplyr::data_frame(participant = eaf_xml %>% xml2::xml_find_all(expr) %>% xml2::xml_attr("PARTICIPANT"),
                          language = eaf_xml %>% xml2::xml_find_all(expr) %>% xml2::xml_attr("LANG_REF")) %>%
                mutate(session_name = gsub(".+/(.+).eaf$", "\\1", eaf_file))

        }
