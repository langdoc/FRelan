#' read_cmdi Function reads a CMDI file
#'
#' @param cmdi_file The path to CMDI file
#' @keywords ELAN
#' @export
#' @examples
#' read_cmdi('file.cmdi')

read_cmdi <- function(cmdi_file){ # this defines the function
        read_xml(cmdi_file) %>% # reads the xml
                xml_find_all('//cmd:Actor') %>% # finds all Actor nodes
                map(~ tibble(participant = .x %>% xml_find_first('./cmd:Code') %>% xml_text,
                             session_name = .x %>% xml_find_first('../../cmd:Name') %>% xml_text,
                             year_birth = .x %>% xml_find_first('./cmd:BirthDate') %>% xml_text,
                             year_rec = .x %>% xml_find_first('../../cmd:Date') %>% xml_text,
                             role = .x %>% xml_find_first('./cmd:Role') %>% xml_text,
                             sex = .x %>% xml_find_first('./cmd:Sex') %>% xml_text,
                             session_address = .x %>% xml_find_first('../../cmd:Location/cmd:Address') %>% xml_text,
                             session_country = .x %>% xml_find_first('../../cmd:Location/cmd:Country') %>% xml_text,
                             session_location = paste0(session_address, ', ', session_country),
                             education = .x %>% xml_find_first('./cmd:Education') %>% xml_text,
                             name_full = .x %>% xml_find_first('./cmd:FullName') %>% xml_text)) %>%
                bind_rows() # After everything is collected into tibble/dataframe,
        # we can just bind the rows together
}
