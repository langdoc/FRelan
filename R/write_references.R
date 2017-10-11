write_references <- function(elan_file = '/Volumes/langdoc/langs/kpv//kpv_izva20140331-1burki/kpv_izva20140331-1burki.eaf'){
        `%>%` <- dplyr::`%>%`

        texts <- xml2::read_xml(elan_file) %>%
                xml2::xml_find_all("//TIER[@LINGUISTIC_TYPE_REF='refT']/ANNOTATION/*/ANNOTATION_VALUE")

        session_name <- stringr::str_extract(elan_file, '(?<=/)[^/]+(?=.eaf$)')
        session_numbers <- stringr::str_pad(1:{texts %>% xml2::xml_text() %>% length()}, width = 4, pad = '0')

        texts %>% xml2::xml_set_text(glue::glue('{session_name}.{session_numbers}')) %>%
                xml2::xml_find_all('/') %>%
                xml2::write_xml('new_test.eaf')

}
