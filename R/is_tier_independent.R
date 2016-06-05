#' is_tier_independent Function checks whether a tier is independent or not
#'
#' This function reads just an individual tier of one ELAN file
#' @param eaf_file This is a path to the file.
#' @param tier This is the linguistic type of the tier
#' @keywords ELAN
#' @export
#' @examples
#' is_tier_independent(path = "path/to/elan/file.eaf", tier = "ref@NTP-M-1986")

is_tier_independent <- function(eaf_file, tier) {

        match <- xml2::read_xml(eaf_file) %>%
                xml2::xml_find_all(xpath = paste0("//TIER[@TIER_ID='", tier, "']/ANNOTATION/*")) %>% xml2::xml_attr("TIME_SLOT_REF1")
        if (length(match) >= 1){
                TRUE
        } else {
                FALSE
        }

}
