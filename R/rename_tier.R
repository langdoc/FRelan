#' rename_tier Function renames a tier
#'
#' This function adds a new tier into an ELAN file. It is prototypical at best.
#' @param filename ELAN file
#' @param current_name Name of the linguistic type
#' @param new_name Name to be set
#' @param suffix Appends a suffix into saved filename
#' @keywords ELAN
#' @export


rename_tier <- function(filename, current_name, new_name, suffix){
        read_xml(filename) %>%
                xml_find_all(glue("//TIER[@TIER_ID='{current_name}']")) %>%
                walk(~ xml_set_attr(.x, 'LINGUISTIC_TYPE_ID', new_name)) %>%
                xml_find_first('/') %>%
                write_xml(str_replace(filename, '.eaf', glue('{suffix}.eaf')))
}
