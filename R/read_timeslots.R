#' read_timeslots Function grabs the time slot information from ELAN file
#'
#' This function reads the information about time slots
#' @param eaf_file This is a path to the file.
#' @keywords ELAN
#' @export
#' @examples
#' read_timeslots(eaf_file = "path/to/elan/file.eaf")

read_timeslots <- function(eaf_file) {

        `%>%` <- dplyr::`%>%`

        time_slots <- xml2::read_xml(eaf_file) %>% xml2::xml_find_all("//TIME_ORDER/TIME_SLOT")

        dplyr::data_frame(time_slot_id = time_slots %>% xml2::xml_attr("TIME_SLOT_ID"),
                          time_value = time_slots %>% xml2::xml_attr("TIME_VALUE") %>% as.numeric)

}
