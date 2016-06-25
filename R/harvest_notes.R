#' harvest_notes Function
#'
#' This function finds and parses note annotation files written in Emacs ld-notes-mode.
#' @param path Where to look for files
#' @keywords language documentation
#' @export

harvest_notes <- function(path = "."){

notes <- list.files(path = path, pattern = "ldnotes\\....", recursive = T, full.names = T)

`%>%` <- dplyr::`%>%`

get_header_attr <- function(note_file){

        # http://stackoverflow.com/questions/30153194/access-name-of-rmd-file-and-use-in-r
        lines <- readLines(note_file)
        # Find the header portion contained between the --- lines.
        header_line_nums <- which(lines == "---") + c(1, -1)
        # Create a string of just that header portion
        header <- paste(lines[seq(header_line_nums[1],
                                  header_line_nums[2])],
                        collapse = "\n")
        # parse it as yaml, which returns a list of property values
        header <- yaml::yaml.load(header)
        dplyr::data_frame(session_number = header$session_number,
                          location = header$session_location,
                          file = note_file)
}

note_headers <- plyr::ldply(notes, get_header_attr)


note_text <- plyr::llply(notes, function(x) {
        readr::read_tsv(x, skip = 10, col_names = F) %>% dplyr::rename(time_stamp = X1, annotation = X2) %>%
                dplyr::mutate(file = x)
})

note_content <- plyr::ldply(note_text, function(x){
        dplyr::left_join(x, note_headers, by = "file") %>% tbl_df
})

note_content %>% dplyr::tbl_df %>% dplyr::filter(! is.na(annotation)) %>%
        dplyr::mutate(tag = stringr::str_replace(annotation, "(^[A-Z]+) (.+)", "\\1")) -> note_content

note_content[stringi::stri_detect_regex(note_content$annotation, "([A-Z]+) .+") == FALSE,]$tag <- NA

note_content %>% dplyr::mutate(annotation = stringr::str_replace(annotation, "(^[A-Z]+) (.+)", "\\2")) -> note_content

note_content

}
