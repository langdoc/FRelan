#' is_tier_independent Function checks whether a linguistic type is independent or not
#'
#' This function checks the linguistic type definition and returns information about whether it is possible to align the tier or not.
#' @param eaf_file This is a path to the file.
#' @param linguistic_type This is the linguistic type
#' @keywords ELAN
#' @export
#' @examples
#' is_tier_independent(path = "path/to/elan/file.eaf", tier = "refT")

is_type_independent <- function(eaf_file, linguistic_type) {

        alignable <- xml2::read_xml(eaf_file) %>%
                xml2::xml_find_all(xpath = paste0("//LINGUISTIC_TYPE[@LINGUISTIC_TYPE_ID='", linguistic_type, "']")) %>%
                xml2::xml_attr("TIME_ALIGNABLE")

        if (alignable == "true"){
                TRUE
        } else {
                FALSE
        }

}
