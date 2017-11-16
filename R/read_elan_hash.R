#' read_elan_hash Function reads the identifier of the ELAN file
#'
#' This function adds a new tier into an ELAN file. It is prototypical at best.
#' @param elan_file ELAN file
#' @keywords ELAN
#' @export

read_elan_hash <- function(elan_file){
        `%>%` <- dplyr::`%>%`
        xml2::read_xml(elan_file) %>% xml2::xml_find_first("//PROPERTY[@NAME='URN']") %>% xml2::xml_text()
}
