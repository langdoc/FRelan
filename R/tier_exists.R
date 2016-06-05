#' tier_exists Function
#'
#' This function simply tests whether ELAN file has a tier or not
#' @param eaf_file The path to ELAN file which we want to parse
#' @param tier Tier name
#' @keywords ELAN
#' @export
#' @examples
#' tier_exists("path/to/elan/file.eaf", "orth@NTP-M-1986")

tier_exists <- function(eaf_file, tier){

        match <- xml2::read_xml(eaf_file) %>%
                xml2::xml_find_all(xpath = paste0("//TIER[@TIER_ID='", tier, "']"))
        if (length(match) >= 1){
                TRUE
        } else {
                FALSE
        }
}

