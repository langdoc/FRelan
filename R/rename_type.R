#' rename_type Function Renames a linguistic type
#'
#' This function rewrites the content of reference tier in an ELAN file
#' @param eaf_file This is a path to the file.
#' @keywords ELAN
#' @export
#' @examples
#' rename_type(eaf_file = "./corpus/elan_file.eaf", current_name = 'refT', new_name = 'referenceT', suffix = '-test')

rename_type <- function(filename, current_name, new_name, suffix){
        read_xml(filename) %>%
                xml_find_all(glue("//TIER[@LINGUISTIC_TYPE_REF='{current_name}']")) %>%
                walk(~ xml_set_attr(.x, 'LINGUISTIC_TYPE_REF', new_name)) %>%
                xml_find_all("../LINGUISTIC_TYPE[@LINGUISTIC_TYPE_ID='{current_name}']") %>%
                walk(~ xml_set_attr(.x, 'LINGUISTIC_TYPE_ID', new_name)) %>%
                xml_find_first('/') %>%
                write_xml(str_replace(filename, '.eaf', glue('{suffix}.eaf')))
}
