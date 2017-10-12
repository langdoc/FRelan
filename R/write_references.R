#' write_references Function read individual tiers from single ELAN files.
#'
#' This function rewrites the content of reference tier in an ELAN file
#' @param eaf_file This is a path to the file.
#' @keywords ELAN
#' @export
#' @examples
#' write_references(eaf_file = "./corpus/elan_file.eaf")

write_references <- function(eaf_file = '~/langdoc/kpv//kpv_izva20140331-1burki/kpv_izva20140331-1burki.eaf'){

        `%>%` <- dplyr::`%>%`

        # This gets current content of the reference tiers

        texts <- xml2::read_xml(eaf_file) %>%
                xml2::xml_find_all("//TIER[@LINGUISTIC_TYPE_REF='refT']/ANNOTATION/*/ANNOTATION_VALUE")

        # This picks up the session name from the filename

        session_name <- stringr::str_extract(eaf_file, '(?<=/)[^/]+(?=.eaf$)')

        # Number of annotations

        number_of_annotations <- texts %>% xml2::xml_text() %>% length()

        # Padding length so we know how many zeros are needed

        pad_length <- nchar(as.character(number_of_annotations))

        # This creates a character vector which has as many elements as there are
        # annotations on reference tiers

        session_numbers <- stringr::str_pad(1:number_of_annotations, width = pad_length, pad = '0')

        # This replaces the annotation content, goes back to root node and writes the file

        texts %>% xml2::xml_set_text(glue::glue('{session_name}.{session_numbers}')) %>%
                xml2::xml_find_all('/') %>%
                xml2::write_xml(eaf_file)

        # This is bit redundant but I didn't come up with better way now. It reads the file,
        # doubles the amount of space and indentation and saves the file again. There is a
        # problem otherwise that the number of spaces doesn't match with the original and
        # what ELAN produces

        readr::read_file(eaf_file) %>% stringr::str_replace_all('  ', '    ') %>% readr::write_file(eaf_file)

}
