#' tier_has_content Function
#'
#' This function simply tests whether ELAN file has a tier or not
#' @param eaf_file The path to ELAN file which we want to parse
#' @param tier Tier name
#' @param value Whether the function should return the number of items on the tier instead of a boolean value
#' @keywords ELAN
#' @export
#' @examples
#' tier_exists("path/to/elan/file.eaf", "orth@NTP-M-1986")

tier_has_content <- function(eaf_file, tier, value = F){

        if (tier_exists(eaf_file = eaf_file, tier = tier) == F){
                print("Tier doesn't exist!")
        } else {
                tier_content <- xml2::read_xml(eaf_file) %>%
                        xml2::xml_find_all(
                                xpath = paste0("//TIER[@TIER_ID='", tier, "']/ANNOTATION/*/ANNOTATION_VALUE")) %>%
                        xml2::xml_text()
        }

        if (value == FALSE && tier_exists(eaf_file = eaf_file, tier = tier) == T) {
                result <- length(tier_content) > 0
                print(result)
        }

        if (value == T && tier_exists(eaf_file = eaf_file, tier = tier) == T) {
                length(tier_content)
        }
}
